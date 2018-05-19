{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.App where

import Control.Applicative (Alternative(..))
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
