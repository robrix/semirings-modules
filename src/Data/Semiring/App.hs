{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.App where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Generics (Generic, Generic1)

newtype App f a = App { getApp :: f a }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)

instance Foldable f => Foldable (App f) where
  foldMap f = foldMap f . getApp

instance Functor f => Functor (App f) where
  fmap f = App . fmap f . getApp

instance Traversable f => Traversable (App f) where
  traverse f = fmap App . traverse f . getApp

instance Applicative f => Applicative (App f) where
  pure = App . pure
  App f <*> App a = App (f <*> a)

instance Alternative f => Alternative (App f) where
  empty = App empty
  App a <|> App b = App (a <|> b)

instance Monad f => Monad (App f) where
  App a >>= f = App (a >>= getApp . f)

instance MonadFix f => MonadFix (App f) where
  mfix f = App (mfix (getApp . f))


-- $
-- Associativity of '<>':
--
-- prop> a <> (b <> c) == (a <> b) <> (c :: App [] Boolean)
instance (Applicative f, Semigroup a) => Semigroup (App f a) where
  App a <> App b = App (liftA2 (<>) a b)

-- $
-- Identity of '<>':
--
-- prop> zero <> a == (a :: App [] Boolean)
-- prop> a <> zero == (a :: App [] Boolean)
instance (Applicative f, Monoid a) => Monoid (App f a) where
  mempty = App (pure mempty)


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Boolean (Boolean(..))
-- >>> import Data.Semiring.Class (zero)
-- >>> instance Arbitrary (f a) => Arbitrary (App f a) where arbitrary = App <$> arbitrary ; shrink (App f) = map App (shrink f)
