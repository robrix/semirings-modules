{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- | Semiring expressing zero, one, or arbitrarily many.
module Data.Semiring.Few
(
-- * Semigroup
  Few(..)
) where

import Data.Data (Data(..))
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic(..))

-- | A 'Semiring' expressing quantities of zero, one, or arbitrarily many.
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

instance Unital Few where
  one = One
