
-- |
-- Module     : Simulation.Aivika.Branch.Br
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module defines 'BrIO' as an instance of the 'MonadDES' type class.
--
module Simulation.Aivika.Branch.Br
       (BrIO,
        runBr,
        branchLevel) where

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Generator
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.QueueStrategy

import Simulation.Aivika.Branch.Internal.Br
import Simulation.Aivika.Branch.Event
import Simulation.Aivika.Branch.Generator
import Simulation.Aivika.Branch.Ref.Base
import Simulation.Aivika.Branch.QueueStrategy

instance MonadDES BrIO

instance MonadComp BrIO
