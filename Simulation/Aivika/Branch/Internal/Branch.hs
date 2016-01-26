
-- |
-- Module     : Simulation.Aivika.Branch.Internal.Branch
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines a branching computation.
--
module Simulation.Aivika.Branch.Internal.Branch
       (BrParams(..),
        Br(..),
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
newtype Br a = Br { unBr :: BrParams -> IO a
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
             brParent :: Maybe BrParams
             -- ^ The branch parent.
           }

instance Monad Br where

  {-# INLINE return #-}
  return = Br . const . return

  {-# INLINE (>>=) #-}
  (Br m) >>= k = Br $ \ps ->
    m ps >>= \a ->
    let m' = unBr (k a) in m' ps

instance Applicative Br where

  {-# INLINE pure #-}
  pure = return

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Functor Br where

  {-# INLINE fmap #-}
  fmap f (Br m) = Br $ fmap f . m 

instance MonadIO Br where

  {-# INLINE liftIO #-}
  liftIO = Br . const . liftIO

instance MonadException Br where

  catchComp (Br m) h = Br $ \ps ->
    catch (m ps) (\e -> unBr (h e) ps)

  finallyComp (Br m1) (Br m2) = Br $ \ps ->
    finally (m1 ps) (m2 ps)
  
  throwComp e = Br $ \ps ->
    throw e

-- | Invoke the computation.
invokeBr :: BrParams -> Br a -> IO a
{-# INLINE invokeBr #-}
invokeBr ps (Br m) = m ps

-- | Run the branching computation.
runBr :: Br a -> IO a
runBr m =
  do ps <- newRootBrParams
     unBr m ps

-- | Create a new child branch.
newBrParams :: BrParams -> IO BrParams
newBrParams ps =
  do id <- atomicModifyIORef (brIdGenerator ps) $ \a ->
       let b = a + 1 in b `seq` (b, b)
     let level = 1 + brLevel ps
     return BrParams { brId = id,
                       brIdGenerator = brIdGenerator ps,
                       brLevel = level `seq` level,
                       brParent = Just ps }

-- | Create a root branch.
newRootBrParams :: IO BrParams
newRootBrParams =
  do genId <- newIORef 0
     return BrParams { brId = 0,
                       brIdGenerator = genId,
                       brLevel = 1,
                       brParent = Nothing
                     }

-- | Return the current branch level starting from 1.
branchLevel :: Br Int
branchLevel = Br $ \ps -> return (brLevel ps)
