{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables #-}
-- | The 'Semigroup' from 'Num'’s '+' and '*' operators, @0@, and @1@.
module Data.Semiring.Arith
(
-- * Numeric 'Semigroup's
  Arith(..)
) where

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Data (Data(..))
import Data.Function (fix)
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic, Generic1)

-- | The 'Semigroup' from 'Num'’s '+' and '*' operators, @0@, and @1@.
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


instance Num r => Semigroup (Arith r) where
  Arith a <> Arith b = Arith (a + b)

instance Num r => Monoid (Arith r) where
  mempty = 0

instance Num r => Semiring (Arith r) where
  (><) = (*)

instance Num r => Unital (Arith r) where
  one = 1
