{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Zip where

import Control.Applicative (Alternative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Align (Align(..))
import Data.Data (Data(..))
import GHC.Generics (Generic, Generic1)

newtype Zip a = Zip { getZip :: [a] }
  deriving (Data, Eq, Generic, Generic1, Ord, Read, Show)

instance Foldable Zip where
  foldMap f = foldMap f . getZip

instance Functor Zip where
  fmap f = Zip . fmap f . getZip

instance Traversable Zip where
  traverse f = fmap Zip . traverse f . getZip

instance Applicative Zip where
  pure = Zip . pure
  Zip f <*> Zip a = Zip (f <*> a)

instance Align Zip where
  nil = Zip nil
  alignWith f (Zip a) (Zip b) = Zip (alignWith f a b)

instance Alternative Zip where
  empty = Zip empty
  Zip a <|> Zip b = Zip (a <|> b)

instance Monad Zip where
  Zip a >>= f = Zip (a >>= getZip . f)

instance MonadFix Zip where
  mfix f = Zip (mfix (getZip . f))
