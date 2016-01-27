
-- |
-- Module     : Simulation.Aivika.Branch
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- This module re-exports the library functionality related to branching computations.
--
module Simulation.Aivika.Branch
       (-- * Modules
        module Simulation.Aivika.Branch.Event,
        module Simulation.Aivika.Branch.Generator,
        module Simulation.Aivika.Branch.Ref.Base,
        -- * Branches
        BrIO,
        runBr,
        branchLevel) where

import Simulation.Aivika.Branch.Internal.Br
import Simulation.Aivika.Branch.Generator
import Simulation.Aivika.Branch.Event
import Simulation.Aivika.Branch.Ref.Base
