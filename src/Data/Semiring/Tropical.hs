{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Tropical where

import Data.Data (Data(..))
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
