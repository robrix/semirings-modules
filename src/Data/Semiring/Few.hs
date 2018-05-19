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

-- | In addition to satisfying the usual laws, 'Few'’s 'Semigroup' instance is idempotent.
--
-- Associativity of '<>':
--
-- prop> a <> (b <> c) == (a <> b) <> (c :: Few)
--
-- Idempotence of '<>':
--
-- prop> a <> a == (a :: Few)
instance Semigroup Few where
  Zero <> b    = b
  a    <> Zero = a
  _    <> _    = More

-- $
-- Identity of '<>':
--
-- prop> zero <> a == (a :: Few)
-- prop> a <> zero == (a :: Few)
instance Monoid Few where
  mempty = Zero

-- $
-- Commutativity of '<>':
--
-- prop> a <> b == b <> (a :: Few)
--
-- Associativity of '><':
--
-- prop> a >< (b >< c) == (a >< b) >< (c :: Few)
--
-- Distributivity of '><' over '<>':
--
-- prop> a >< (b <> c) == (a >< b) <> (a >< c :: Few)
-- prop> (a <> b) >< c == (a >< c) <> (b >< c :: Few)
--
-- Absorption of '><' by 'zero':
--
-- prop> a >< zero == (zero :: Few)
-- prop> zero >< a == (zero :: Few)
instance Semiring Few where
  Zero ><    _ = Zero
  _    >< Zero = Zero
  One  >< One  = One
  _    >< _    = More

instance Unital Few where
  one = One

-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..), elements)
-- >>> instance Arbitrary Few where arbitrary = elements [Zero .. More] ; shrink few = case few of Zero -> [] ; One -> [Zero] ; More -> [Zero, One]
