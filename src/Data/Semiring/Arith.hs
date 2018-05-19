{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- | The 'Semiring' from 'Num'’s '+' and '*' operators, @0@, and @1@.
module Data.Semiring.Arith
(
-- * Numeric 'Semiring's
  Arith(..)
) where

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Data (Data(..))
import Data.Function (fix)
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic, Generic1)

-- | The 'Semiring' from 'Num'’s '+' and '*' operators, @0@, and @1@.
newtype Arith r = Arith { getArith :: r }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)

instance Enum r => Enum (Arith r) where
  succ (Arith a) = Arith (succ a)
  pred (Arith a) = Arith (pred a)
  toEnum = Arith . toEnum
  fromEnum = fromEnum . getArith
  enumFrom (Arith a) = Arith <$> enumFrom a
  enumFromThen (Arith a) (Arith b) = Arith <$> enumFromThen a b
  enumFromTo (Arith a) (Arith b) = Arith <$> enumFromTo a b
  enumFromThenTo (Arith a) (Arith b) (Arith c) = Arith <$> enumFromThenTo a b c

instance Num r => Num (Arith r) where
  Arith a + Arith b = Arith (a + b)
  Arith a * Arith b = Arith (a * b)
  Arith a - Arith b = Arith (a - b)
  negate (Arith a) = Arith (negate a)
  abs    (Arith a) = Arith (abs    a)
  signum (Arith a) = Arith (signum a)
  fromInteger      = Arith . fromInteger

instance Foldable Arith where
  foldMap f = f . getArith

instance Functor Arith where
  fmap f (Arith a) = Arith (f a)

instance Traversable Arith where
  traverse f (Arith a) = Arith <$> f a

instance Applicative Arith where
  pure = Arith
  a <* _ = a
  _ *> a = a
  Arith f <*> Arith a = Arith (f a)
  liftA2 f (Arith a) (Arith b) = Arith (f a b)

instance Monad Arith where
  (>>) = (*>)
  Arith a >>= f = f a

instance MonadFix Arith where
  mfix f = fix (f . getArith)


-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: Arith Integer)
instance Num r => Semigroup (Arith r) where
  (<>) = (+)

-- $
-- Identity of '<>':
-- prop> zero <> a == (a :: Arith Integer)
-- prop> a <> zero == (a :: Arith Integer)
instance Num r => Monoid (Arith r) where
  mempty = 0

-- $
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: Arith Integer)
--
-- Associativity of '><':
-- prop> a >< (b >< c) == (a >< b) >< (c :: Arith Integer)
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: Arith Integer)
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: Arith Integer)
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero == (zero :: Arith Integer)
-- prop> zero >< a == (zero :: Arith Integer)
instance Num r => Semiring (Arith r) where
  (><) = (*)

-- $
-- Identity of '><':
-- prop> one >< a = (a :: Arith Integer)
-- prop> a >< one = (a :: Arith Integer)
instance Num r => Unital (Arith r) where
  one = 1


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Class (zero)
-- >>> instance Arbitrary r => Arbitrary (Arith r) where arbitrary = Arith <$> arbitrary ; shrink (Arith r) = map Arith (shrink r)
