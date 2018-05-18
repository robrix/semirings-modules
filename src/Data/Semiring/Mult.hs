{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-- | The 'Semigroup' from '><' & 'Monoid' with 'one'.
module Data.Semiring.Mult
(
-- * Multiplicative 'Semigroup's
  Mult(..)
) where

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Coerce (coerce)
import Data.Data (Data(..))
import Data.Function (fix)
import Data.Ix (Ix(..))
import Data.Semiring.Class (Semiring(..), Unital(..))
import GHC.Generics (Generic, Generic1)

-- | The multiplicative 'Semigroup' of 'Semiring's, and multiplicative 'Monoid' of 'Unital' 'Semiring's.
newtype Mult r = Mult { getMult :: r }
  deriving (Bounded, Data, Eq, Generic, Generic1, Ix, Num, Ord, Read, Show)

instance Enum r => Enum (Mult r) where
  succ = coerce
  pred = coerce
  toEnum = coerce (toEnum :: Int -> r)
  fromEnum = coerce (fromEnum :: r -> Int)
  enumFrom = coerce (enumFrom :: r -> [r])
  enumFromThen = coerce (enumFromThen :: r -> r -> [r])
  enumFromTo = coerce (enumFromTo :: r -> r -> [r])
  enumFromThenTo = coerce (enumFromThenTo :: r -> r -> r -> [r])

instance Foldable Mult where
  foldMap = coerce

instance Functor Mult where
  fmap = coerce

instance Traversable Mult where
  traverse f (Mult a) = Mult <$> f a

instance Applicative Mult where
  pure = Mult
  a <* _ = a
  _ *> a = a
  (<*>) = coerce
  liftA2 = coerce

instance Monad Mult where
  (>>) = (*>)
  Mult a >>= f = f a

instance MonadFix Mult where
  mfix f = fix (f . getMult)

instance Semiring r => Semigroup (Mult r) where
  (<>) = coerce ((><) :: r -> r -> r)

instance Unital r => Monoid (Mult r) where
  mempty = Mult one
