{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Module.Class where

import Data.Semiring.Class

-- | A left @r@-module over a 'Semiring' @r@.
class (Semiring r, Semigroup m) => Module r m where
  (><<) :: r -> m -> m

  infixl 7 ><<
