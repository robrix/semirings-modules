{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Tropical where

import Control.Applicative (Applicative(..))
import Data.Data (Data(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic, Generic1)

data Tropical n = Finite n | Infinity
  deriving (Data, Eq, Generic, Generic1, Ord, Read, Show)

instance Real r => Num (Tropical r) where
  Finite a + Finite b = Finite (a `min` b)
  a        + Infinity = a
  Infinity + b        = b
  Finite a * Finite b = Finite (a + b)
  Infinity * _        = Infinity
  _        * Infinity = Infinity
  negate (Finite a) = Finite (negate a)
  negate Infinity   = Infinity
  abs    (Finite a) = Finite (abs    a)
  abs    Infinity   = Infinity
  signum (Finite a) = Finite (signum a)
  signum Infinity   = 1
  fromInteger       = Finite . fromInteger

instance Foldable Tropical where
  foldMap f (Finite a) = f a
  foldMap _ Infinity   = mempty

instance Functor Tropical where
  fmap f (Finite a) = Finite (f a)
  fmap _ Infinity   = Infinity

instance Traversable Tropical where
  traverse f (Finite a) = Finite <$> f a
  traverse _ Infinity   = pure Infinity

instance Applicative Tropical where
  pure = Finite
  a <* _ = a
  _ *> a = a
  Finite f <*> Finite a = Finite (f a)
  _        <*> _        = Infinity
  liftA2 f (Finite a) (Finite b) = Finite (f a b)
  liftA2 _ _          _          = Infinity

instance Monad Tropical where
  (>>) = (*>)
  Finite a >>= f = f a
  Infinity >>= _ = Infinity


-- | The tropical semigroup is defined by taking '<>' = 'min'.
--
-- Associativity of '<>':
--
-- prop> a <> (b <> c) == (a <> b) <> (c :: Tropical Integer)
--
-- Idempotence of '<>':
--
-- prop> a <> a == (a :: Tropical Integer)
instance Ord r => Semigroup (Tropical r) where
  Finite a <> Finite b = Finite (a `min` b)
  Infinity <> b        = b
  a        <> Infinity = a

-- $
-- Identity of '<>':
--
-- prop> zero <> a == (a :: Tropical Integer)
-- prop> a <> zero == (a :: Tropical Integer)
instance Ord r => Monoid (Tropical r) where
  mempty = Infinity

-- | The tropical semiring is defined by taking '<>' = 'min' and '><' = '<>'.
--
-- Commutativity of '<>':
--
-- prop> a >< b = b >< (a :: Tropical (Set Char))
--
-- Associativity of '><':
--
-- prop> a >< (b >< c) == (a >< b) >< (c :: Tropical (Set Char))
--
-- Distributivity of '><' over '<>':
--
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: Tropical (Set Char))
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: Tropical (Set Char))
--
-- Absorption of '><' by 'zero':
--
-- prop> a >< zero == (zero :: Tropical (Set Char))
-- prop> zero >< a == (zero :: Tropical (Set Char))
instance (Ord r, Semigroup r) => Semiring (Tropical r) where
  Finite a >< Finite b = Finite (a <> b)
  _        >< _        = Infinity

-- $
-- Identity of '><':
--
-- prop> one >< a = (a :: Tropical (Set Char))
-- prop> a >< one = (a :: Tropical (Set Char))
instance (Ord r, Semigroup r) => Unital (Tropical r) where
  one = Infinity


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Class (zero)
-- >>> import Data.Set (Set)
-- >>> instance Arbitrary r => Arbitrary (Tropical r) where arbitrary = Finite <$> arbitrary ; shrink (Finite r) = map Finite (shrink r) ; shrink Infinity = []
