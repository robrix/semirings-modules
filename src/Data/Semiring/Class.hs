module Data.Semiring.Class where

-- | The identity element of some 'Monoid' @m@.
--
--   If @m@ is additionally a 'Semiring', this is its additive identity.
zero :: Monoid m => m
zero = mempty

-- | A 'Semiring' is an abstract algebraic structure consisting of a commutative 'Monoid' and an associative operator '(><)', with the additional constraints that '(><)' distributes over '(<>)' and that 'zero' is the annihilator for '(><)'.
class Monoid r => Semiring r where
  infixr 7 ><
  (><) :: r -> r -> r
