
{-# LANGUAGE BangPatterns #-}

-- |
-- Module     : Simulation.Aivika.Branch.Internal.Ref.Lazy
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The implementation of lazy mutable references.
--
module Simulation.Aivika.Branch.Internal.Ref.Lazy
       (Ref,
        newEmptyRef,
        newEmptyRef0,
        newRef,
        newRef0,
        readRef,
        writeRef,
        modifyRef) where

-- import Debug.Trace

import Data.IORef
import qualified Data.IntMap as M

import System.Mem.Weak

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.Branch.Internal.BR

-- | A reference map.
type RefMap a = IORef (M.IntMap (IORef a, Weak (IORef ())))

-- | A lazy mutable reference.
data Ref a = Ref { refMap :: RefMap a,
                   -- ^ the map of actual references
                   refWeakMap :: Weak (RefMap a)
                   -- ^ a weak reference to the map itself
                 }

instance Eq (Ref a) where
  r1 == r2 = (refMap r1) == (refMap r2)

-- | Create an empty reference.
newEmptyRef :: Simulation (BR IO) (Ref a)
newEmptyRef = Simulation $ const newEmptyRef0

-- | Create an empty reference.
newEmptyRef0 :: BR IO (Ref a)
newEmptyRef0 =
  BR $ \ps ->
  do rm <- newIORef M.empty
     wm <- mkWeakIORef rm $
           -- trace ("fin newEmptyRef0: " ++ show (brId ps)) $
           finalizeRef rm
     return Ref { refMap = rm,
                  refWeakMap = wm }

-- | Create a new reference.
newRef :: a -> Simulation (BR IO) (Ref a)
newRef = Simulation . const . newRef0

-- | Create a new reference.
newRef0 :: a -> BR IO (Ref a)
newRef0 a =
  BR $ \ps ->
  do r  <- invokeBR ps newEmptyRef0
     ra <- newIORef a
     let !i  = brId ps
         !wm = refWeakMap r
     -- mkWeakIORef (brUniqueRef ps) (trace ("fin newIORef0: " ++ show i) $ finalizeCell wm i)
     wa <- mkWeakIORef (brUniqueRef ps) (finalizeCell wm i)
     writeIORef (refMap r) $
       M.insert i (ra, wa) M.empty
     return r
     
-- | Read the value of a reference.
readRef :: Ref a -> Event (BR IO) a
readRef r =
  Event $ \p ->
  BR $ \ps ->
  do m <- readIORef (refMap r)
     let loop ps =
           case M.lookup (brId ps) m of
             Just (ra, wa) -> readIORef ra
             Nothing ->
               case brParent ps of
                 Just ps' -> loop ps'
                 Nothing  -> error "Cannot find branch: readRef"
     loop ps

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Event (BR IO) ()
writeRef r a =
  Event $ \p ->
  BR $ \ps ->
  do m <- readIORef (refMap r)
     let !i = brId ps
     case M.lookup i m of
       Just (ra, wa) -> writeIORef ra a
       Nothing ->
         do ra <- newIORef a
            let !wm = refWeakMap r
            -- mkWeakIORef (brUniqueRef ps) (trace ("fin writeRef: " ++ show i) $ finalizeCell wm i)
            wa <- mkWeakIORef (brUniqueRef ps) (finalizeCell wm i)
            atomicModifyIORef (refMap r) $ \m ->
              let m' = M.insert i (ra, wa) m in (m', ())

-- | Mutate the contents of the reference.
modifyRef :: Ref a -> (a -> a) -> Event (BR IO) ()
modifyRef r f =
  Event $ \p ->
  BR $ \ps ->
  do m <- readIORef (refMap r)
     let !i = brId ps
     case M.lookup i m of
       Just (ra, wa) ->
         do a <- readIORef ra
            let b = f a
            writeIORef ra b
       Nothing ->
         do a <- invokeBR ps $ invokeEvent p $ readRef r
            invokeBR ps $ invokeEvent p $ writeRef r (f a)

-- | Finalize the reference.
finalizeRef :: RefMap a -> IO ()
finalizeRef r =
  do m <- readIORef r
     forM_ (M.elems m) $ \(ra, wa) ->
       finalize wa

-- | Finalize the reference cell by the specified branch identifier.
finalizeCell :: Weak (RefMap a) -> Int -> IO ()
finalizeCell wm i =
  do rm <- deRefWeak wm
     -- trace ("finalizeRef: " ++ show i) $ return ()
     case rm of
       Nothing ->
         return ()
       Just rm ->
         do m <- readIORef rm
            case M.lookup i m of
              Just (ra, wa) ->
                atomicModifyIORef rm $ \m ->
                let m' = M.delete i m in (m', ())
              Nothing ->
                return ()
