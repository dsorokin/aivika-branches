
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

import Data.IORef
import qualified Data.IntMap as M

import System.Mem.Weak

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.Branch.Internal.Branch

-- | A reference map.
type RefMap a = IORef (M.IntMap (Weak (IORef a)))

-- | A mutable reference.
data Ref a = Ref { refMap :: RefMap a,
                   -- ^ the map of actual references
                   refWeakMap :: Weak (RefMap a)
                   -- ^ a weak reference to the map itself
                 }

instance Eq (Ref a) where
  r1 == r2 = (refMap r1) == (refMap r2)

-- | Create an empty reference.
newEmptyRef :: Simulation Br (Ref a)
newEmptyRef = Simulation $ const newEmptyRef0

-- | Create an empty reference.
newEmptyRef0 :: Br (Ref a)
newEmptyRef0 =
  Br $ \ps ->
  do rm <- newIORef M.empty
     wm <- mkWeakIORef rm $
           do m <- readIORef rm
              forM_ (M.elems m) finalize
     return Ref { refMap = rm,
                  refWeakMap = wm }

-- | Create a new reference.
newRef :: a -> Simulation Br (Ref a)
newRef = Simulation . const . newRef0

-- | Create a new reference.
newRef0 :: a -> Br (Ref a)
newRef0 a =
  Br $ \ps ->
  do r  <- invokeBr ps newEmptyRef0
     ra <- newIORef a
     let !i  = brId ps
         !wm = refWeakMap r
     wa <- mkWeak ps ra (Just $ finalizeRef wm i)
     writeIORef (refMap r) $
       M.insert i wa M.empty
     return r
     
-- | Read the value of a reference.
readRef :: Ref a -> Event Br a
readRef r =
  Event $ \p ->
  Br $ \ps ->
  do m <- readIORef (refMap r)
     let loop ps =
           case M.lookup (brId ps) m of
             Just wa ->
               do ra <- deRefWeak wa
                  case ra of
                    Just ra -> readIORef ra
                    Nothing -> error "The reference was finalized: readRef"
             Nothing ->
               case brParent ps of
                 Just ps' -> loop ps'
                 Nothing  -> error "Cannot find branch: readRef"
     loop ps

-- | Write a new value into the reference.
writeRef :: Ref a -> a -> Event Br ()
writeRef r a =
  Event $ \p ->
  Br $ \ps ->
  do m <- readIORef (refMap r)
     let !i = brId ps
     case M.lookup i m of
       Just wa ->
         do ra <- deRefWeak wa
            case ra of
              Just ra -> a `seq` writeIORef ra a
              Nothing -> error "The reference was finalized: writeRef"
       Nothing ->
         do ra <- a `seq` newIORef a
            let !wm = refWeakMap r
            wa <- mkWeak ps ra (Just $ finalizeRef wm i)
            atomicModifyIORef (refMap r) $ \m ->
              let m' = M.insert i wa m in (m', ())

-- | Mutate the contents of the reference.
modifyRef :: Ref a -> (a -> a) -> Event Br ()
modifyRef r f =
  Event $ \p ->
  Br $ \ps ->
  do m <- readIORef (refMap r)
     let !i = brId ps
     case M.lookup i m of
       Just wa ->
         do ra <- deRefWeak wa
            case ra of
              Just ra ->
                do a <- readIORef ra
                   let b = f a
                   b `seq` writeIORef ra b
              Nothing -> error "The reference was finalized: modifyRef"
       Nothing ->
         do a <- invokeBr ps $ invokeEvent p $ readRef r
            invokeBr ps $ invokeEvent p $ writeRef r (f a)

-- | Finalize the reference cell by the specified branch identifier.
finalizeRef :: Weak (RefMap a) -> Int -> IO ()
finalizeRef wm i =
  do rm <- deRefWeak wm
     case rm of
       Nothing ->
         return ()
       Just rm ->
         do m <- readIORef rm
            case M.lookup i m of
              Just wa ->
                do atomicModifyIORef rm $ \m ->
                     let m' = M.delete i m in (m', ())
                   finalize wa
              Nothing ->
                return ()
