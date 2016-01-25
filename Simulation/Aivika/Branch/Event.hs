
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Branch.Event
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The module defines an event queue.
--
module Simulation.Aivika.Branch.Event
       (branchEvent,
        branchEventParallel,
        futureEvent) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import qualified Simulation.Aivika.PriorityQueue.Pure as PQ

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.Branch.Internal.Branch

-- | An implementation of the 'EventQueueing' type class.
instance EventQueueing Br where

  -- | The event queue type.
  data EventQueue Br =
    EventQueue { queuePQ :: IORef (PQ.PriorityQueue (Point Br -> Br ())),
                 -- ^ the underlying priority queue
                 queueBusy :: IORef Bool,
                 -- ^ whether the queue is currently processing events
                 queueTime :: IORef Double
                 -- ^ the actual time of the event queue
               }

  newEventQueue specs =
    do f  <- liftIO $ newIORef False
       t  <- liftIO $ newIORef (spcStartTime specs)
       pq <- liftIO $ newIORef PQ.emptyQueue
       return EventQueue { queuePQ   = pq,
                           queueBusy = f,
                           queueTime = t }

  enqueueEvent t (Event m) =
    Event $ \p ->
    Br $ \ps ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in modifyIORef pq $ \x -> PQ.enqueue x t m

  runEventWith processing (Event e) =
    Dynamics $ \p ->
    do invokeDynamics p $ processEvents processing
       e p

  eventQueueCount =
    Event $ \p ->
    Br $ \ps ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in fmap PQ.queueCount $ readIORef pq

-- | Process the pending events.
processPendingEventsCore :: Bool -> Dynamics Br ()
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    Br $ \ps ->
    do let q = runEventQueue $ pointRun p
           f = queueBusy q
       f' <- readIORef f
       unless f' $
         do writeIORef f True
            call q p ps
            writeIORef f False
  call q p ps =
    do let pq = queuePQ q
           r  = pointRun p
       f <- fmap PQ.queueNull $ readIORef pq
       unless f $
         do (t2, c2) <- fmap PQ.queueFront $ readIORef pq
            let t = queueTime q
            t' <- readIORef t
            when (t2 < t') $ 
              error "The time value is too small: processPendingEventsCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentEvents && (t2 == pointTime p))) $
              do writeIORef t t2
                 modifyIORef pq PQ.dequeue
                 let sc = pointSpecs p
                     t0 = spcStartTime sc
                     dt = spcDT sc
                     n2 = fromIntegral $ floor ((t2 - t0) / dt)
                 invokeBr ps $
                   c2 $ p { pointTime = t2,
                            pointIteration = n2,
                            pointPhase = -1 }
                 call q p ps

-- | Process the pending events synchronously, i.e. without past.
processPendingEvents :: Bool -> Dynamics Br ()
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    Br $ \ps ->
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeBr ps $
              invokeDynamics p $
              processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent :: Dynamics Br ()
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier :: Dynamics Br ()
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore :: Dynamics Br ()
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore :: Dynamics Br ()
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: EventProcessing -> Dynamics Br ()
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore

-- | Branch a new computation and return its result leaving the current computation intact.
--
-- A new derivative branch with level increased by 1 is created at the current modeling time.
-- Then the result of the specified computation for the derivative branch is returned.
--
-- If the branch level exceeds 'branchMaxLevel' then 'Nothing' is returned.
branchEvent :: Event Br a -> Event Br (Maybe a)
branchEvent (Event m) =
  Event $ \p ->
  Br $ \ps->
  if brLevel ps >= brMaxLevel ps
  then return Nothing
  else do p2  <- clonePoint p
          ps2 <- newBrParams ps
          a   <- invokeBr ps2 (m p2)
          return (Just a)

-- | Like 'branchEvent' but allows creating multiple derivative branches.
branchEventParallel :: [Event Br a] -> Event Br (Maybe [a])
branchEventParallel ms =
  Event $ \p ->
  Br $ \ps ->
  if brLevel ps >= brMaxLevel ps
  then return Nothing
  else do as <-
            forM ms $ \(Event m) ->
            do p2  <- clonePoint p
               ps2 <- newBrParams ps
               invokeBr ps2 (m p2)
          return (Just as)

-- | Branch a new computation and return its result at the desired time
-- in the future leaving the current computation intact.
--
-- A new derivative branch with level increased by 1 is created at the current modeling time.
-- All pending events are processed till the specified time for that new branch. Then the result
-- of the specified computation for the derivative branch is returned.
--
-- If either the branch level exceeds 'branchMaxLevel' or the specified time is greater
-- than 'stoptime' then 'Nothing' is returned.
futureEvent :: Double -> Event Br a -> Event Br (Maybe a)
futureEvent t (Event m) =
  Event $ \p ->
  Br $ \ps ->
  let sc = pointSpecs p
      t0 = spcStartTime sc
      t' = spcStopTime sc
      dt = spcDT sc
      n  = fromIntegral $ floor ((t - t0) / dt)
  in if (brLevel ps >= brMaxLevel ps) || (t > t')
     then return Nothing
     else do p2  <- clonePoint p
             ps2 <- newBrParams ps
             a <- invokeBr ps $
                  m $ p2 { pointTime = t,
                           pointIteration = n,
                           pointPhase = -1 }
             return (Just a)

-- | Clone the point.
clonePoint :: Point Br -> IO (Point Br)
clonePoint p =
  do let r = pointRun p
         q = runEventQueue r
     pq  <- readIORef (queuePQ q)
     t   <- readIORef (queueTime q)
     pq2 <- newIORef pq
     f2  <- newIORef False
     t2  <- newIORef t
     let q2 = EventQueue { queuePQ   = pq2,
                           queueBusy = f2,
                           queueTime = t2 }
         r2 = r { runEventQueue = q2 }
         p2 = p { pointRun = r2 }
     return p2