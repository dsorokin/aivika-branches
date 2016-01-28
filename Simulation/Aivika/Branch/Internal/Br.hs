
-- |
-- Module     : Simulation.Aivika.Branch.Internal.Br
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines a branching computation.
--
module Simulation.Aivika.Branch.Internal.Br
       (BrParams(..),
        BrIO(..),
        invokeBr,
        runBr,
        newBrParams,
        newRootBrParams,
        branchLevel) where

import Data.IORef
import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception (throw, catch, finally)

import Simulation.Aivika.Trans.Exception

-- | The branching computation.
newtype BrIO a = Br { unBr :: BrParams -> IO a
                      -- ^ Unwrap the computation.
                    }

-- | The parameters of the computation.
data BrParams =
  BrParams { brId :: !Int,
             -- ^ The branch identifier.
             brIdGenerator :: IORef Int,
             -- ^ The generator of identifiers.
             brLevel :: !Int,
             -- ^ The branch level.
             brParent :: Maybe BrParams,
             -- ^ The branch parent.
             brUniqueRef :: IORef ()
             -- ^ The unique reference to which
             -- the finalizers are attached to
             -- be garbage collected.
           }

instance Monad BrIO where

  {-# INLINE return #-}
  return = Br . const . return

  {-# INLINE (>>=) #-}
  (Br m) >>= k = Br $ \ps ->
    m ps >>= \a ->
    let m' = unBr (k a) in m' ps

instance Applicative BrIO where

  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Functor BrIO where

  {-# INLINE fmap #-}
  fmap f (Br m) = Br $ fmap f . m 

instance MonadIO BrIO where

  {-# INLINE liftIO #-}
  liftIO = Br . const . liftIO

instance MonadException BrIO where

  catchComp (Br m) h = Br $ \ps ->
    catch (m ps) (\e -> unBr (h e) ps)

  finallyComp (Br m1) (Br m2) = Br $ \ps ->
    finally (m1 ps) (m2 ps)
  
  throwComp e = Br $ \ps ->
    throw e

-- | Invoke the computation.
invokeBr :: BrParams -> BrIO a -> IO a
{-# INLINE invokeBr #-}
invokeBr ps (Br m) = m ps

-- | Run the branching computation.
runBr :: BrIO a -> IO a
runBr m =
  do ps <- newRootBrParams
     unBr m ps

-- | Create a new child branch.
newBrParams :: BrParams -> IO BrParams
newBrParams ps =
  do id <- atomicModifyIORef (brIdGenerator ps) $ \a ->
       let b = a + 1 in b `seq` (b, b)
     let level = 1 + brLevel ps
     uniqueRef <- newIORef ()
     return BrParams { brId = id,
                       brIdGenerator = brIdGenerator ps,
                       brLevel = level `seq` level,
                       brParent = Just ps,
                       brUniqueRef = uniqueRef }

-- | Create a root branch.
newRootBrParams :: IO BrParams
newRootBrParams =
  do genId <- newIORef 0
     uniqueRef <- newIORef ()
     return BrParams { brId = 0,
                       brIdGenerator = genId,
                       brLevel = 1,
                       brParent = Nothing,
                       brUniqueRef = uniqueRef
                     }

-- | Return the current branch level starting from 1.
branchLevel :: BrIO Int
branchLevel = Br $ \ps -> return (brLevel ps)
