
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Branch.QueueStrategy
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines some queue strategy instances.
--
module Simulation.Aivika.Branch.QueueStrategy where

import Control.Monad.Trans

import Simulation.Aivika.Trans
import qualified Simulation.Aivika.Trans.DoubleLinkedList as LL

import Simulation.Aivika.Branch.Internal.Br
import Simulation.Aivika.Branch.Ref.Base

-- | An implementation of the 'FCFS' queue strategy.
instance QueueStrategy BrIO FCFS where

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue BrIO FCFS a = FCFSQueue (LL.DoubleLinkedList BrIO a)

  newStrategyQueue s = fmap FCFSQueue LL.newList

  strategyQueueNull (FCFSQueue q) = LL.listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance DequeueStrategy BrIO FCFS where

  strategyDequeue (FCFSQueue q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance EnqueueStrategy BrIO FCFS where

  strategyEnqueue (FCFSQueue q) i = LL.listAddLast q i

-- | An implementation of the 'LCFS' queue strategy.
instance QueueStrategy BrIO LCFS where

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue BrIO LCFS a = LCFSQueue (LL.DoubleLinkedList BrIO a)

  newStrategyQueue s = fmap LCFSQueue LL.newList
       
  strategyQueueNull (LCFSQueue q) = LL.listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance DequeueStrategy BrIO LCFS where

  strategyDequeue (LCFSQueue q) =
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance EnqueueStrategy BrIO LCFS where

  strategyEnqueue (LCFSQueue q) i = LL.listInsertFirst q i
