{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Alt where

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
