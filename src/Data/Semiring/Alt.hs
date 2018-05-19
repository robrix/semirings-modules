{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Alt where

import Control.Applicative (Alternative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Generics (Generic, Generic1)

newtype Alt f a = Alt { getAlt :: f a }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)

instance Foldable f => Foldable (Alt f) where
  foldMap f = foldMap f . getAlt

instance Functor f => Functor (Alt f) where
  fmap f = Alt . fmap f . getAlt

instance Traversable f => Traversable (Alt f) where
  traverse f = fmap Alt . traverse f . getAlt

instance Applicative f => Applicative (Alt f) where
  pure = Alt . pure
  Alt f <*> Alt a = Alt (f <*> a)

instance Alternative f => Alternative (Alt f) where
  empty = Alt empty
  Alt a <|> Alt b = Alt (a <|> b)

instance Monad f => Monad (Alt f) where
  Alt a >>= f = Alt (a >>= getAlt . f)

instance MonadFix f => MonadFix (Alt f) where
  mfix f = Alt (mfix (getAlt . f))


-- $
-- Associativity of '<>':
--
-- prop> a <> (b <> c) == (a <> b) <> (c :: Alt Maybe Int)
instance Alternative f => Semigroup (Alt f a) where
  (<>) = (<|>)

-- $
-- Identity of '<>':
--
-- prop> zero <> a == (a :: App Maybe Int)
-- prop> a <> zero == (a :: App Maybe Int)
instance Alternative f => Monoid (Alt f a) where
  mempty = empty


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Class (zero)
-- >>> instance Arbitrary (f a) => Arbitrary (Alt f a) where arbitrary = Alt <$> arbitrary ; shrink (Alt f) = map Alt (shrink f)
