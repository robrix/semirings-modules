{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- | 'Semiring's from boolean algebras.
module Data.Semiring.Boolean
(
-- * Boolean algebras
  Boolean(..)
) where

import Data.Data (Data(..))
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic)

-- | Boolean algebras form a semiring with '<>' as disjunction, '><' as conjunction, 'zero' as 'False', and 'one' as 'True'.
newtype Boolean = Boolean { getBoolean :: Bool }
  deriving (Bounded, Data, Eq, Generic, Ix, Ord, Read, Show)

instance Enum Boolean where
  succ (Boolean a) = Boolean (succ a)
  pred (Boolean a) = Boolean (pred a)
  toEnum = Boolean . toEnum
  fromEnum = fromEnum . getBoolean
  enumFrom (Boolean a) = Boolean <$> enumFrom a
  enumFromThen (Boolean a) (Boolean b) = Boolean <$> enumFromThen a b
  enumFromTo (Boolean a) (Boolean b) = Boolean <$> enumFromTo a b
  enumFromThenTo (Boolean a) (Boolean b) (Boolean c) = Boolean <$> enumFromThenTo a b c


-- $
-- Associativity of '<>':
--
-- prop> a <> (b <> c) == (a <> b) <> (c :: Boolean)
instance Semigroup Boolean where
  Boolean a <> Boolean b = Boolean (a || b)

-- $
-- Identity of '<>':
--
-- prop> zero <> a == (a :: Boolean)
-- prop> a <> zero == (a :: Boolean)
instance Monoid Boolean where
  mempty = Boolean False

-- $
-- Commutativity of '<>':
--
-- prop> a >< b = b >< (a :: Boolean)
--
-- Associativity of '><':
--
-- prop> a >< (b >< c) == (a >< b) >< (c :: Boolean)
--
-- Distributivity of '><' over '<>':
--
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: Boolean)
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: Boolean)
--
-- Absorption of '><' by 'zero':
--
-- prop> a >< zero == (zero :: Boolean)
-- prop> zero >< a == (zero :: Boolean)
instance Semiring Boolean where
  Boolean a >< Boolean b = Boolean (a && b)

-- $
-- Identity of '><':
--
-- prop> one >< a = (a :: Boolean)
-- prop> a >< one = (a :: Boolean)
instance Unital Boolean where
  one = Boolean True


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Class (zero)
-- >>> instance Arbitrary Boolean where arbitrary = Boolean <$> arbitrary ; shrink (Boolean b) = map Boolean (shrink b)
