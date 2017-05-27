
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Branch.Br
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines that 'BR' can be an instance of the 'MonadDES' and 'EventIOQueueing' type classes.
--
module Simulation.Aivika.Branch.BR
       (BR,
        runBR,
        branchLevel) where

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.QueueStrategy

import Simulation.Aivika.IO

import Simulation.Aivika.Branch.Internal.BR
import Simulation.Aivika.Branch.Event
import Simulation.Aivika.Branch.Generator
import Simulation.Aivika.Branch.Ref.Base.Lazy
import Simulation.Aivika.Branch.Ref.Base.Strict
import Simulation.Aivika.Branch.QueueStrategy

instance MonadDES (BR IO)

instance MonadComp (BR IO)

-- | An implementation of the 'EventIOQueueing' type class.
instance EventIOQueueing (BR IO) where

  enqueueEventIO = enqueueEvent
  
