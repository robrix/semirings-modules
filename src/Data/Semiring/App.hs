{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.App where

import Data.Data (Data)
import Data.Ix (Ix)
import GHC.Generics (Generic, Generic1)

newtype App f a = App { getApp :: f a }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Ord, Read, Show)
