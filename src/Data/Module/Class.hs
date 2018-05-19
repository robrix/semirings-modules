{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Module.Class where

import Data.Semiring.Class

class (Semiring r, Semigroup m) => Module r m where
  (><<) :: r -> m -> m

  infixl 7 ><<
