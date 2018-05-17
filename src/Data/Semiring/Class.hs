module Data.Semiring.Class where

-- | The identity element of some 'Monoid' @m@.
--
--   If @m@ is additionally a 'Semiring', this is its additive identity.
zero :: Monoid m => m
zero = mempty

class Monoid r => Semiring r where
  infixr 7 ><
  (><) :: r -> r -> r
