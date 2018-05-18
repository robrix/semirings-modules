{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semiring.Mult where

import Control.Applicative (Applicative(..))
import Data.Coerce (coerce)

newtype Mult r = Mult { getMult :: r }
  deriving (Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)

instance Applicative Mult where
  pure = Mult
  a <* _ = a
  _ *> a = a
  (<*>) = coerce
  liftA2 = coerce
