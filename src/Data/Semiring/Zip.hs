{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Zip where

import Data.Data (Data(..))
import GHC.Generics (Generic, Generic1)

newtype Zip a = Zip { getZip :: [a] }
  deriving (Data, Eq, Generic, Generic1, Ord, Read, Show)

instance Foldable Zip where
  foldMap f = foldMap f . getZip

instance Functor Zip where
  fmap f = Zip . fmap f . getZip
