
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Branch.Event
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The module defines an event queue, where 'BR' can be an instance of 'EventQueueing'.
-- Also it defines basic functions for branching computations.
--
module Simulation.Aivika.Branch.Event
       (branchEvent,
        futureEvent,
        futureEventWith) where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import qualified Simulation.Aivika.PriorityQueue.Pure as PQ

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.Branch.Internal.BR

-- | An implementation of the 'EventQueueing' type class.
instance EventQueueing (BR IO) where

  -- | The event queue type.
  data EventQueue (BR IO) =
    EventQueue { queuePQ :: IORef (PQ.PriorityQueue (Point (BR IO) -> BR IO ())),
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
    BR $ \ps ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in modifyIORef pq $ \x -> PQ.enqueue x t m

  runEventWith processing (Event e) =
    Dynamics $ \p ->
    do invokeDynamics p $ processEvents processing
       e p

  eventQueueCount =
    Event $ \p ->
    BR $ \ps ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in fmap PQ.queueCount $ readIORef pq

-- | Process the pending events.
processPendingEventsCore :: Bool -> Dynamics (BR IO) ()
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    BR $ \ps ->
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
                 invokeBR ps $
                   c2 $ p { pointTime = t2,
                            pointIteration = n2,
                            pointPhase = -1 }
                 call q p ps

-- | Process the pending events synchronously, i.e. without past.
processPendingEvents :: Bool -> Dynamics (BR IO) ()
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    BR $ \ps ->
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeBR ps $
              invokeDynamics p $
              processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent :: Dynamics (BR IO) ()
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier :: Dynamics (BR IO) ()
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore :: Dynamics (BR IO) ()
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore :: Dynamics (BR IO) ()
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: EventProcessing -> Dynamics (BR IO) ()
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore

-- | Branch a new computation and return its result leaving the current computation intact.
--
-- A new derivative branch with 'branchLevel' increased by 1 is created at the current modeling time.
-- Then the result of the specified computation for the derivative branch is returned.
--
-- The state of the current computation including its event queue and mutable references 'Ref'
-- remain intact. In some sense we copy the state of the model to the derivative branch and then
-- proceed with the derived simulation. The copying operation is relatively cheap.
branchEvent :: Event (BR IO) a -> Event (BR IO) a
branchEvent (Event m) =
  Event $ \p ->
  BR $ \ps->
  do p2  <- clonePoint p
     ps2 <- newBRParams ps
     invokeBR ps2 (m p2)

-- | Branch a new computation and return its result at the desired time
-- in the future leaving the current computation intact.
--
-- A new derivative branch with 'branchLevel' increased by 1 is created at the current modeling time.
-- All pending events are processed till the specified time for that new branch. Then the result
-- of the specified computation for the derivative branch is returned.
--
-- The state of the current computation including its event queue and mutable references 'Ref'
-- remain intact. In some sense we copy the state of the model to the derivative branch and then
-- proceed with the derived simulation. The copying operation is relatively cheap.
futureEvent :: Double -> Event (BR IO) a -> Event (BR IO) a
futureEvent = futureEventWith CurrentEvents

-- | Like 'futureEvent' but allows specifying how the pending events must be processed.
futureEventWith :: EventProcessing -> Double -> Event (BR IO) a -> Event (BR IO) a
futureEventWith processing t (Event m) =
  Event $ \p ->
  BR $ \ps ->
  do when (t < pointTime p) $
       error "The specified time is less than the current modeling time: futureEventWith"
     p2  <- clonePoint p
     ps2 <- newBRParams ps
     let sc = pointSpecs p
         t0 = spcStartTime sc
         t' = spcStopTime sc
         dt = spcDT sc
         n  = fromIntegral $ floor ((t - t0) / dt)
         p' = p2 { pointTime = t,
                   pointIteration = n,
                   pointPhase = -1 }
     invokeBR ps2 $
       invokeDynamics p' $
       processEvents processing
     invokeBR ps2 (m p')

-- | Clone the time point.
clonePoint :: Point (BR IO) -> IO (Point (BR IO))
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
