{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables #-}
-- | The 'Semigroup' from '><' & 'Monoid' with 'one'.
module Data.Semiring.Mult
(
-- * Multiplicative 'Semigroup's
  Mult(..)
) where

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Data (Data(..))
import Data.Function (fix)
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic, Generic1)

-- | The multiplicative 'Semigroup' of 'Semiring's, and multiplicative 'Monoid' of 'Unital' 'Semiring's.
newtype Mult r = Mult { getMult :: r }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)

instance Enum r => Enum (Mult r) where
  succ (Mult a) = Mult (succ a)
  pred (Mult a) = Mult (pred a)
  toEnum = Mult . toEnum
  fromEnum = fromEnum . getMult
  enumFrom (Mult a) = Mult <$> enumFrom a
  enumFromThen (Mult a) (Mult b) = Mult <$> enumFromThen a b
  enumFromTo (Mult a) (Mult b) = Mult <$> enumFromTo a b
  enumFromThenTo (Mult a) (Mult b) (Mult c) = Mult <$> enumFromThenTo a b c

instance Num r => Num (Mult r) where
  Mult a + Mult b = Mult (a + b)
  Mult a * Mult b = Mult (a * b)
  Mult a - Mult b = Mult (a - b)
  negate (Mult a) = Mult (negate a)
  abs    (Mult a) = Mult (abs    a)
  signum (Mult a) = Mult (signum a)
  fromInteger     = Mult . fromInteger

instance Foldable Mult where
  foldMap f = f . getMult

instance Functor Mult where
  fmap f (Mult a) = Mult (f a)

instance Traversable Mult where
  traverse f (Mult a) = Mult <$> f a

instance Applicative Mult where
  pure = Mult
  a <* _ = a
  _ *> a = a
  Mult f <*> Mult a = Mult (f a)
  liftA2 f (Mult a) (Mult b) = Mult (f a b)

instance Monad Mult where
  (>>) = (*>)
  Mult a >>= f = f a

instance MonadFix Mult where
  mfix f = fix (f . getMult)


-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: Mult (Arith Integer))
instance Semiring r => Semigroup (Mult r) where
  Mult a <> Mult b = Mult (a >< b)

instance Unital r => Monoid (Mult r) where
  mempty = Mult one


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Arith (Arith(..))
-- >>> instance Arbitrary r => Arbitrary (Mult r) where arbitrary = Mult <$> arbitrary ; shrink (Mult r) = map Mult (shrink r)
-- >>> instance Arbitrary r => Arbitrary (Arith r) where arbitrary = Arith <$> arbitrary ; shrink (Arith r) = map Arith (shrink r)
