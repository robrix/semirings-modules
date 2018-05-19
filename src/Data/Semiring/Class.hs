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
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
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

-- $
-- Identity of '><':
-- prop> one >< a = (a :: ())
-- prop> a >< one = (a :: ())
instance Unital () where
  one = ()


-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) ~= (a <> b) <> (c :: Int -> Set Char)
--
-- Identity of '<>':
-- prop> zero <> a ~= (a :: Int -> Set Char)
-- prop> a <> zero ~= (a :: Int -> Set Char)
--
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: Int -> Set Char)
--
-- Associativity of '><':
-- prop> a >< (b >< c) ~= (a >< b) >< (c :: Int -> Set Char)
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: Int -> Set Char)
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: Int -> Set Char)
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero ~= (zero :: Int -> Set Char)
-- prop> zero >< a ~= (zero :: Int -> Set Char)
instance Semiring b => Semiring (a -> b) where
  (f >< g) a = f a >< g a


-- Semigroup

-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: Dual (Set Char))
--
-- Identity of '<>':
-- prop> zero <> a == (a :: Dual (Set Char))
-- prop> a <> zero == (a :: Dual (Set Char))
--
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: Dual (Set Char))
--
-- Associativity of '><':
-- prop> a >< (b >< c) == (a >< b) >< (c :: Dual (Set Char))
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: Dual (Set Char))
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: Dual (Set Char))
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero == (zero :: Dual (Set Char))
-- prop> zero >< a == (zero :: Dual (Set Char))
instance Semiring r => Semiring (Dual r) where
  Dual a >< Dual b = Dual (b >< a)

-- $
-- Identity of '><':
-- prop> one >< a = (a :: Dual (Arith Integer))
-- prop> a >< one = (a :: Dual (Arith Integer))
instance Unital r => Unital (Dual r) where
  one = Dual one


-- containers

-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: IntMap ())
--
-- Identity of '<>':
-- prop> zero <> a == (a :: IntMap ())
-- prop> a <> zero == (a :: IntMap ())
--
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: IntMap ())
--
-- Associativity of '><':
-- prop> a >< (b >< c) == (a >< b) >< (c :: IntMap ())
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: IntMap ())
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: IntMap ())
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero == (zero :: IntMap ())
-- prop> zero >< a == (zero :: IntMap ())
instance Semiring (IntMap.IntMap a) where
  (><) = IntMap.intersection

-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: IntSet)
--
-- Identity of '<>':
-- prop> zero <> a == (a :: IntSet)
-- prop> a <> zero == (a :: IntSet)
--
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: IntSet)
--
-- Associativity of '><':
-- prop> a >< (b >< c) == (a >< b) >< (c :: IntSet)
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: IntSet)
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: IntSet)
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero == (zero :: IntSet)
-- prop> zero >< a == (zero :: IntSet)
instance Semiring IntSet.IntSet where
  (><) = IntSet.intersection

-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: Map Char ())
--
-- Identity of '<>':
-- prop> zero <> a == (a :: Map Char ())
-- prop> a <> zero == (a :: Map Char ())
--
-- Commutativity of '<>':
-- prop> a >< b = b >< (a :: Map Char ())
--
-- Associativity of '><':
-- prop> a >< (b >< c) == (a >< b) >< (c :: Map Char ())
--
-- Distributivity of '><' over '<>':
-- prop> a >< (b <> c) = (a >< b) <> (a >< c :: Map Char ())
-- prop> (a <> b) >< c = (a >< c) <> (b >< c :: Map Char ())
--
-- Absorption of '><' by 'zero':
-- prop> a >< zero == (zero :: Map Char ())
-- prop> zero >< a == (zero :: Map Char ())
instance Ord k => Semiring (Map.Map k v) where
  (><) = Map.intersection

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
instance Ord a => Semiring (Set.Set a) where
  (><) = Set.intersection


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Test.QuickCheck.Function
-- >>> import Data.Semiring.Arith
-- >>> instance Arbitrary r => Arbitrary (Arith r) where arbitrary = Arith <$> arbitrary ; shrink (Arith r) = map Arith (shrink r)
-- >>> :{
-- infix 4 ~=
-- f ~= g = (==) <$> f <*> g
-- :}
