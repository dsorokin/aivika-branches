
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
        module Simulation.Aivika.Branch.Ref.Base,
        -- * Branches
        Br,
        runBr,
        branchLevel,
        branchMaxLevel) where

import Simulation.Aivika.Branch.Internal.Branch
import Simulation.Aivika.Branch.Event
import Simulation.Aivika.Branch.Ref.Base