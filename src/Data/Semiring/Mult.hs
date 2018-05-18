{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semiring.Mult where

import Control.Applicative (Applicative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Coerce (coerce)
import Data.Function (fix)

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
