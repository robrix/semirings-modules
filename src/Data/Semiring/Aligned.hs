{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Aligned where

import Control.Applicative (Alternative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Generics (Generic, Generic1)

newtype Aligned f a = Aligned { getAlt :: f a }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)

instance Foldable f => Foldable (Aligned f) where
  foldMap f = foldMap f . getAlt

instance Functor f => Functor (Aligned f) where
  fmap f = Aligned . fmap f . getAlt

instance Traversable f => Traversable (Aligned f) where
  traverse f = fmap Aligned . traverse f . getAlt

instance Applicative f => Applicative (Aligned f) where
  pure = Aligned . pure
  Aligned f <*> Aligned a = Aligned (f <*> a)

instance Alternative f => Alternative (Aligned f) where
  empty = Aligned empty
  Aligned a <|> Aligned b = Aligned (a <|> b)

instance Monad f => Monad (Aligned f) where
  Aligned a >>= f = Aligned (a >>= getAlt . f)

instance MonadFix f => MonadFix (Aligned f) where
  mfix f = Aligned (mfix (getAlt . f))


-- $
-- Associativity of '<>':
--
-- prop> a <> (b <> c) == (a <> b) <> (c :: Aligned Maybe Int)
instance Alternative f => Semigroup (Aligned f a) where
  (<>) = (<|>)

-- $
-- Identity of '<>':
--
-- prop> zero <> a == (a :: App Maybe Int)
-- prop> a <> zero == (a :: App Maybe Int)
instance Alternative f => Monoid (Aligned f a) where
  mempty = empty


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Boolean (Boolean(..))
-- >>> import Data.Semiring.Class (zero)
-- >>> instance Arbitrary (f a) => Arbitrary (Aligned f a) where arbitrary = Aligned <$> arbitrary ; shrink (Aligned f) = map Aligned (shrink f)
-- >>> instance Arbitrary Boolean where arbitrary = Boolean <$> arbitrary ; shrink (Boolean b) = map Boolean (shrink b)
