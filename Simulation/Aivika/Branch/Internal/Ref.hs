
{-# LANGUAGE BangPatterns #-}

-- |
-- Module     : Simulation.Aivika.Branch.Internal.Ref
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The implementation of mutable references.
--
module Simulation.Aivika.Branch.Internal.Ref
       (Ref,
        newEmptyRef,
        newEmptyRef0,
        newRef,
        newRef0,
        readRef,
        writeRef,
        modifyRef) where

import Debug.Trace

import Data.IORef
import qualified Data.IntMap as M

import System.Mem.Weak

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.Branch.Internal.Br

-- | A reference map.
type RefMap a = IORef (M.IntMap (IORef a))

-- | A mutable reference.
data Ref a = Ref { refMap :: RefMap a,
                   -- ^ the map of actual references
                   refWeakMap :: Weak (RefMap a)
                   -- ^ a weak reference to the map itself
                 }

instance Eq (Ref a) where
  r1 == r2 = (refMap r1) == (refMap r2)

-- | Create an empty reference.
newEmptyRef :: Simulation BrIO (Ref a)
newEmptyRef = Simulation $ const newEmptyRef0

-- | Create an empty reference.
newEmptyRef0 :: BrIO (Ref a)
newEmptyRef0 =
  Br $ \ps ->
  do rm <- newIORef M.empty
     wm <- mkWeakIORef rm $
           trace "fin newEmptyRef0" $
           atomicModifyIORef rm $ \m -> (M.empty, ())
     return Ref { refMap = rm,
                  refWeakMap = wm }

-- | Create a new reference.
newRef :: a -> Simulation BrIO (Ref a)
newRef = Simulation . const . newRef0

-- | Create a new reference.
newRef0 :: a -> BrIO (Ref a)
newRef0 a =
  Br $ \ps ->
  do r  <- invokeBr ps newEmptyRef0
     ra <- newIORef a
     let !i  = brId ps
         !wm = refWeakMap r
     mkWeakIORef (brIdGenerator ps) (trace "fin newIORef0" $ finalizeRef wm i)
     writeIORef (refMap r) $
       M.insert i ra M.empty
     return r
     
-- | Read the value of a reference.
readRef :: Ref a -> Event BrIO a
readRef r =
  Event $ \p ->
  Br $ \ps ->
  do m <- readIORef (refMap r)
     let loop ps =
           case M.lookup (brId ps) m of
             Just ra -> readIORef ra
             Nothing ->
               case brParent ps of
                 Just ps' -> loop ps'
                 Nothing  -> error "Cannot find branch: readRef"
     loop ps

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Event BrIO ()
writeRef r a =
  Event $ \p ->
  Br $ \ps ->
  do m <- readIORef (refMap r)
     let !i = brId ps
     case M.lookup i m of
       Just ra -> a `seq` writeIORef ra a
       Nothing ->
         do ra <- a `seq` newIORef a
            let !wm = refWeakMap r
            mkWeakIORef (brIdGenerator ps) (trace "fin writeRef" $ finalizeRef wm i)
            atomicModifyIORef (refMap r) $ \m ->
              let m' = M.insert i ra m in (m', ())

-- | Mutate the contents of the reference.
modifyRef :: Ref a -> (a -> a) -> Event BrIO ()
modifyRef r f =
  Event $ \p ->
  Br $ \ps ->
  do m <- readIORef (refMap r)
     let !i = brId ps
     case M.lookup i m of
       Just ra ->
         do a <- readIORef ra
            let b = f a
            b `seq` writeIORef ra b
       Nothing ->
         do a <- invokeBr ps $ invokeEvent p $ readRef r
            invokeBr ps $ invokeEvent p $ writeRef r (f a)

-- | Finalize the reference cell by the specified branch identifier.
finalizeRef :: Weak (RefMap a) -> Int -> IO ()
finalizeRef wm i =
  do rm <- deRefWeak wm
     trace ("finalizeRef: " ++ show i) $
       return ()
     case rm of
       Nothing ->
         return ()
       Just rm ->
         do m <- readIORef rm
            case M.lookup i m of
              Just ra ->
                atomicModifyIORef rm $ \m ->
                let m' = M.delete i m in (m', ())
              Nothing ->
                return ()
