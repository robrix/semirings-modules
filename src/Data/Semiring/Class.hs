module Data.Semiring.Class where

-- | The identity element of some 'Monoid' @m@.
--
--   If @m@ is additionally a 'Semiring', this is its additive identity.
zero :: Monoid m => m
zero = mempty

-- | A 'Semiring' @r@ is an abstract algebraic structure consisting of a commutative 'Semigroup' and an associative operator '(><)', with the additional constraints that '(><)' distributes over '(<>)'. Additionally, if @s@ is a 'Monoid', 'zero' is the absorping element of '(><)'.
--
--   Laws:
--
--   Associativity of '<>' (the 'Semigroup' law):
--
-- @
--   a '<>' (b '<>' c) = (a '<>' b) '<>' c
-- @
--
--   Identity of '<>' (the 'Monoid' law, if @s@ is a 'Monoid'):
--
-- @
--   'zero' '<>' a = a
--   a '<>' 'zero' = a
-- @
--
--   Commutativity  of '<>':
--
-- @
--   a '<>' b = b '<>' a
-- @
--
--   Associativity of '><':
--
-- @
--   a '><' (b '><' c) = (a '><' b) '><' c
-- @
--
--   Distributivity of '><' over '<>':
--
-- @
--   a '><' (b '<>' c) = (a '><' b) '<>' (a '><' c)
--   (a '<>' b) '><' c = (a '><' c) '<>' (b '><' c)
-- @
--
--   Absorption of '><' by 'zero' (if @s@ is a 'Monoid'):
--
-- @
--   'zero' '><' a = 'zero'
--   a '><' 'zero' = 'zero'
-- @
class Semigroup r => Semiring r where
  infixr 7 ><
  (><) :: r -> r -> r


-- | 'Unital' 'Semiring's additionally have a multiplicative identity, 'one'.
--
--   Laws:
--
--   Identity of '><':
--
--   > one >< a = a
--   > a >< one = a
class Semiring r => Unital r where
  one :: r
