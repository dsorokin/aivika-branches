
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Branch.Ref.Base
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- The implementation of mutable references.
--
module Simulation.Aivika.Branch.Ref.Base where

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Ref.Base

import Simulation.Aivika.Branch.Internal.Branch
import qualified Simulation.Aivika.Branch.Internal.Ref as R

-- | The implementation of mutable references.
instance MonadRef BrIO where

  -- | The mutable reference.
  newtype Ref BrIO a = Ref { refValue :: R.Ref a }

  {-# INLINE newRef #-}
  newRef = fmap Ref . R.newRef 

  {-# INLINE readRef #-}
  readRef (Ref r) = R.readRef r

  {-# INLINE writeRef #-}
  writeRef (Ref r) = R.writeRef r

  {-# INLINE modifyRef #-}
  modifyRef (Ref r) = R.modifyRef r

  {-# INLINE equalRef #-}
  equalRef (Ref r1) (Ref r2) = (r1 == r2)

-- | A subtype of mutable references that can be created under more weak conditions.
instance MonadRef0 BrIO where

  {-# INLINE newRef0 #-}
  newRef0 = fmap Ref . R.newRef0
