
{-# LANGUAGE RecursiveDo #-}

-- |
-- Module     : Simulation.Aivika.Branch.Internal.BR
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines a branching computation.
--
module Simulation.Aivika.Branch.Internal.BR
       (BRParams(..),
        BR(..),
        invokeBR,
        runBR,
        newBRParams,
        newRootBRParams,
        branchLevel) where

import Data.IORef
import Data.Maybe

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Exception (throw, catch, finally)

import Simulation.Aivika.Trans.Exception

-- | The branching computation.
newtype BR m a = BR { unBR :: BRParams -> m a
                      -- ^ Unwrap the computation.
                    }

-- | The parameters of the computation.
data BRParams =
  BRParams { brId :: !Int,
             -- ^ The branch identifier.
             brIdGenerator :: IORef Int,
             -- ^ The generator of identifiers.
             brLevel :: !Int,
             -- ^ The branch level.
             brParent :: Maybe BRParams,
             -- ^ The branch parent.
             brUniqueRef :: IORef ()
             -- ^ The unique reference to which
             -- the finalizers are attached to
             -- be garbage collected.
           }

instance Monad m => Monad (BR m) where

  {-# INLINE return #-}
  return = BR . const . return

  {-# INLINE (>>=) #-}
  (BR m) >>= k = BR $ \ps ->
    m ps >>= \a ->
    let m' = unBR (k a) in m' ps

instance Applicative m => Applicative (BR m) where

  {-# INLINE pure #-}
  pure = BR . const . pure

  {-# INLINE (<*>) #-}
  (BR f) <*> (BR m) = BR $ \ps -> f ps <*> m ps

instance Functor m => Functor (BR m) where

  {-# INLINE fmap #-}
  fmap f (BR m) = BR $ fmap f . m 

instance MonadIO m => MonadIO (BR m) where

  {-# INLINE liftIO #-}
  liftIO = BR . const . liftIO

instance MonadTrans BR where

  {-# INLINE lift #-}
  lift = BR . const

instance MonadFix m => MonadFix (BR m) where

  mfix f = 
    BR $ \ps ->
    do { rec { a <- invokeBR ps (f a) }; return a }

instance MonadException m => MonadException (BR m) where

  catchComp (BR m) h = BR $ \ps ->
    catchComp (m ps) (\e -> unBR (h e) ps)

  finallyComp (BR m1) (BR m2) = BR $ \ps ->
    finallyComp (m1 ps) (m2 ps)
  
  throwComp e = BR $ \ps ->
    throwComp e

-- | Invoke the computation.
invokeBR :: BRParams -> BR m a -> m a
{-# INLINE invokeBR #-}
invokeBR ps (BR m) = m ps

-- | Run the branching computation.
runBR :: MonadIO m => BR m a -> m a
{-# INLINABLE runBR #-}
runBR m =
  do ps <- liftIO newRootBRParams
     unBR m ps

-- | Create a new child branch.
newBRParams :: BRParams -> IO BRParams
newBRParams ps =
  do id <- atomicModifyIORef (brIdGenerator ps) $ \a ->
       let b = a + 1 in b `seq` (b, b)
     let level = 1 + brLevel ps
     uniqueRef <- newIORef ()
     return BRParams { brId = id,
                       brIdGenerator = brIdGenerator ps,
                       brLevel = level `seq` level,
                       brParent = Just ps,
                       brUniqueRef = uniqueRef }

-- | Create a root branch.
newRootBRParams :: IO BRParams
newRootBRParams =
  do genId <- newIORef 0
     uniqueRef <- newIORef ()
     return BRParams { brId = 0,
                       brIdGenerator = genId,
                       brLevel = 0,
                       brParent = Nothing,
                       brUniqueRef = uniqueRef
                     }

-- | Return the current branch level starting from 0.
branchLevel :: Monad m => BR m Int
{-# INLINABLE branchLevel #-}
branchLevel = BR $ \ps -> return (brLevel ps)
