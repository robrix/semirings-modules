{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Tropical where

import Data.Data (Data(..))
import GHC.Generics (Generic, Generic1)

data Tropical n = Finite n | Infinity
  deriving (Data, Eq, Generic, Generic1, Ord, Read, Show)
