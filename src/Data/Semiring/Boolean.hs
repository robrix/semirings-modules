{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Boolean
(
-- * Boolean algebras
  Boolean(..)
) where

import Data.Data (Data(..))
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic)

-- | Boolean algebras form a semiring with '<>' as disjunction, '><' as conjunction, 'zero' as 'False', and 'one' as 'True'.
newtype Boolean = Boolean { getBoolean :: Bool }
  deriving (Bounded, Data, Eq, Generic, Ix, Ord, Read, Show)

instance Enum Boolean where
  succ (Boolean a) = Boolean (succ a)
  pred (Boolean a) = Boolean (pred a)
  toEnum = Boolean . toEnum
  fromEnum = fromEnum . getBoolean
  enumFrom (Boolean a) = Boolean <$> enumFrom a
  enumFromThen (Boolean a) (Boolean b) = Boolean <$> enumFromThen a b
  enumFromTo (Boolean a) (Boolean b) = Boolean <$> enumFromTo a b
  enumFromThenTo (Boolean a) (Boolean b) (Boolean c) = Boolean <$> enumFromThenTo a b c
