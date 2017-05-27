
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Branch.QueueStrategy
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines queue strategies 'FCFS' and 'LCFS' for the 'BR' computation.
--
module Simulation.Aivika.Branch.QueueStrategy () where

import Control.Monad.Trans

import Simulation.Aivika.Trans
import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL

import Simulation.Aivika.Branch.Internal.BR
import Simulation.Aivika.Branch.Ref.Base

-- | An implementation of the 'FCFS' queue strategy.
instance QueueStrategy (BR IO) FCFS where

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue (BR IO) FCFS a = FCFSQueue (LL.DoubleLinkedList (BR IO) a)

  newStrategyQueue s = fmap FCFSQueue LL.newList

  strategyQueueNull (FCFSQueue q) = LL.listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance DequeueStrategy (BR IO) FCFS where

  strategyDequeue (FCFSQueue q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance EnqueueStrategy (BR IO) FCFS where

  strategyEnqueue (FCFSQueue q) i = LL.listAddLast q i

-- | An implementation of the 'LCFS' queue strategy.
instance QueueStrategy (BR IO) LCFS where

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue (BR IO) LCFS a = LCFSQueue (LL.DoubleLinkedList (BR IO) a)

  newStrategyQueue s = fmap LCFSQueue LL.newList
       
  strategyQueueNull (LCFSQueue q) = LL.listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance DequeueStrategy (BR IO) LCFS where

  strategyDequeue (LCFSQueue q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance EnqueueStrategy (BR IO) LCFS where

  strategyEnqueue (LCFSQueue q) i = LL.listInsertFirst q i
