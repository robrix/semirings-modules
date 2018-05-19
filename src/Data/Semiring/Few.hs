{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Few where

import Data.Data (Data(..))
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..))
import GHC.Generics (Generic(..))

data Few = Zero | One | More
  deriving (Bounded, Data, Enum, Eq, Generic, Ix, Ord, Show)

instance Semigroup Few where
  Zero <> b    = b
  a    <> Zero = a
  _    <> _    = More

instance Monoid Few where
  mempty = Zero

instance Semiring Few where
  Zero ><    _ = Zero
  _    >< Zero = Zero
  One  >< One  = One
  _    >< _    = More
