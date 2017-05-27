
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Branch.Ref.Base.Strict
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.3
--
-- Here is an implementation of strict mutable references, where
-- 'BR' can be an instance of 'MonadRef' and 'MonadRef0'.
--
module Simulation.Aivika.Branch.Ref.Base.Strict () where

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Ref.Base.Strict

import Simulation.Aivika.Branch.Internal.BR
import qualified Simulation.Aivika.Branch.Internal.Ref.Strict as R

-- | The implementation of mutable references.
instance MonadRef (BR IO) where

  -- | The mutable reference.
  newtype Ref (BR IO) a = Ref { refValue :: R.Ref a }

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
instance MonadRef0 (BR IO) where

  {-# INLINE newRef0 #-}
  newRef0 = fmap Ref . R.newRef0
