{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Alt where

import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Generics (Generic, Generic1)

newtype Alt f a = Alt { getAlt :: f a }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)

instance Foldable f => Foldable (Alt f) where
  foldMap f = foldMap f . getAlt
