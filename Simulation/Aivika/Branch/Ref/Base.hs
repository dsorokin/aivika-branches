
-- |
-- Module     : Simulation.Aivika.Branch.Ref.Base
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- Here is an implementation of strict mutable references, where
-- 'BR' can be an instance of 'MonadRef' and 'MonadRef0'.
--
module Simulation.Aivika.Branch.Ref.Base 
       (module Simulation.Aivika.Branch.Ref.Base.Strict) where

import Simulation.Aivika.Trans.Ref.Base

import Simulation.Aivika.Branch.Internal.BR
import Simulation.Aivika.Branch.Ref.Base.Strict
