{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Few where

import Data.Data (Data(..))
import Data.Ix (Ix(..))
import GHC.Generics (Generic(..))

data Few = Zero | One | More
  deriving (Bounded, Data, Enum, Eq, Generic, Ix, Ord, Show)
