{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Aligned where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Align (Align(..))
import Data.Data (Data)
import Data.Ix (Ix)
import Data.Semiring.Class (Semiring(..))
import Data.These (mergeThese)
import GHC.Generics (Generic, Generic1)

newtype Aligned f a = Aligned { getAligned :: f a }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)

instance Foldable f => Foldable (Aligned f) where
  foldMap f = foldMap f . getAligned

instance Functor f => Functor (Aligned f) where
  fmap f = Aligned . fmap f . getAligned

instance Traversable f => Traversable (Aligned f) where
  traverse f = fmap Aligned . traverse f . getAligned

instance Applicative f => Applicative (Aligned f) where
  pure = Aligned . pure
  Aligned f <*> Aligned a = Aligned (f <*> a)

instance Alternative f => Alternative (Aligned f) where
  empty = Aligned empty
  Aligned a <|> Aligned b = Aligned (a <|> b)

instance Align f => Align (Aligned f) where
  nil = Aligned nil
  alignWith f (Aligned a) (Aligned b) = Aligned (alignWith f a b)

instance Monad f => Monad (Aligned f) where
  Aligned a >>= f = Aligned (a >>= getAligned . f)

instance MonadFix f => MonadFix (Aligned f) where
  mfix f = Aligned (mfix (getAligned . f))


-- $
-- Associativity of '<>':
--
-- prop> a <> (b <> c) == (a <> b) <> (c :: Aligned Maybe Boolean)
instance (Align f, Semigroup a) => Semigroup (Aligned f a) where
  (<>) = alignWith (mergeThese (<>))

-- $
-- Identity of '<>':
--
-- prop> zero <> a == (a :: Aligned Maybe Boolean)
-- prop> a <> zero == (a :: Aligned Maybe Boolean)
instance (Align f, Semigroup a) => Monoid (Aligned f a) where
  mempty = nil

-- $
-- Commutativity of '<>':
--
-- prop> a <> b == b <> (a :: Aligned Maybe Boolean)
--
-- Associativity of '><':
--
-- prop> a >< (b >< c) == (a >< b) >< (c :: Aligned Maybe Boolean)
--
-- Distributivity of '><' over '<>':
--
-- prop> a >< (b <> c) == (a >< b) <> (a >< c :: Aligned Maybe Boolean)
-- prop> (a <> b) >< c == (a >< c) <> (b >< c :: Aligned Maybe Boolean)
--
-- Absorption of '><' by 'zero':
--
-- prop> a >< zero == (zero :: Aligned Maybe Boolean)
-- prop> zero >< a == (zero :: Aligned Maybe Boolean)
instance (Align f, Applicative f, Semiring a) => Semiring (Aligned f a) where
  (><) = liftA2 (><)


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Boolean (Boolean(..))
-- >>> import Data.Semiring.Class (zero)
-- >>> instance Arbitrary (f a) => Arbitrary (Aligned f a) where arbitrary = Aligned <$> arbitrary ; shrink (Aligned f) = map Aligned (shrink f)
-- >>> instance Arbitrary Boolean where arbitrary = Boolean <$> arbitrary ; shrink (Boolean b) = map Boolean (shrink b)
