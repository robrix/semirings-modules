module Data.Semiring.Class where

-- | The identity element of some 'Monoid' @m@.
--
--   If @m@ is additionally a 'Semiring', this is its additive identity.
zero :: Monoid m => m
zero = mempty

-- | A 'Semiring' is an abstract algebraic structure consisting of a commutative 'Monoid' and an associative operator '(><)', with the additional constraints that '(><)' distributes over '(<>)' and that 'zero' is the annihilator for '(><)'.
--
--   Laws:
--
--   Associativity of '<>' (the 'Semigroup' law):
--
--   > a <> (b <> c) = (a <> b) <> c
--
--   Identity of '<>' (the 'Monoid' law):
--
--   > zero <> a = a
--   > a <> zero = a
--
--   Commutativity  of '<>':
--
--   > a <> b = b <> a
--
--   Associativity of '><':
--
--   > a >< (b >< c) = (a >< b) >< c
--
--   Distributivity of '><' over '<>':
--
--   > a >< (b <> c) = (a >< b) <> (a >< c)
--   > (a <> b) >< c = (a >< c) <> (b >< c)
--
--   Absorption of '><' into 'zero':
--
--   > zero >< a = zero
--   > a >< zero = zero
class Monoid r => Semiring r where
  infixr 7 ><
  (><) :: r -> r -> r
