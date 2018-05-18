{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Data.Semiring.Mult where

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.Semiring.Class

-- | The multiplicative 'Semigroup' of 'Semiring's, and multiplicative 'Monoid' of 'Unital' 'Semiring's.
newtype Mult r = Mult { getMult :: r }
  deriving (Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

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
