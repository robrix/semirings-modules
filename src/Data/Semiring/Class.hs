-- | 'Semiring's, with and without multiplicative identities.
module Data.Semiring.Class
(
-- * Semirings
  Semiring(..)
, zero
-- * Unital semirings
, Unital(..)
) where

import Data.Semigroup as Semigroup
import qualified Data.Set as Set

-- | A 'Semiring' @r@ is an abstract algebraic structure consisting of a commutative 'Semigroup' and an associative operator '><', with the additional constraints that '><' distributes over '<>'. Additionally, if @s@ is a 'Monoid', 'zero' is the absorping element of '><'.
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
--   'zero' '<>' a    = a
--   a    '<>' 'zero' = a
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
--   'zero' '><' a    = 'zero'
--   a    '><' 'zero' = 'zero'
-- @
class Semigroup r => Semiring r where
  infixr 7 ><
  (><) :: r -> r -> r

-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: ())
--
-- Identity of '<>':
-- prop> zero <> a == (a :: ())
-- prop> a <> zero == (a :: ())
--
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: ())
--
-- Associativity of '><':
-- prop> a >< (b >< c) == (a >< b) >< (c :: ())
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: ())
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: ())
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero == (zero :: ())
-- prop> zero >< a == (zero :: ())
instance Semiring () where
  (><) = (<>)


-- | The identity element of some 'Monoid' @m@.
--
--   If @m@ is additionally a 'Semiring', this is its additive identity.
zero :: Monoid m => m
zero = mempty


-- | 'Unital' 'Semiring's additionally have a multiplicative identity, 'one'.
--
--   Laws:
--
--   Identity of '><':
--
-- @
--   'one' '><' a   = a
--   a   '><' 'one' = a
-- @
class Semiring r => Unital r where
  one :: r


-- base

-- $
-- Identity of '><':
-- prop> one >< a = (a :: ())
-- prop> a >< one = (a :: ())
instance Unital () where
  one = ()


-- Semigroup

instance Semiring r => Semiring (Dual r) where
  Dual a >< Dual b = Dual (b >< a)

instance Unital r => Unital (Dual r) where
  one = Dual one


-- containers

-- | Sets form a 'Semiring' under 'intersection'.
instance Ord a => Semiring (Set.Set a) where
  (><) = Set.intersection

-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: Set Char)
--
-- Identity of '<>':
-- prop> zero <> a == (a :: Set Char)
-- prop> a <> zero == (a :: Set Char)
--
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: Set Char)
--
-- Associativity of '><':
-- prop> a >< (b >< c) == (a >< b) >< (c :: Set Char)
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: Set Char)
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: Set Char)
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero == (zero :: Set Char)
-- prop> zero >< a == (zero :: Set Char)


-- $setup
-- >>> import Test.QuickCheck ()
