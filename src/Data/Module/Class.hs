{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.Module.Class where

import Data.Semiring.Class (Semiring(..))

-- | A left @r@-module over a 'Semiring' @r@.
--
--   Laws:
--
--   Associativity of '<>' (the 'Semigroup' law):
--
-- @
--   a '<>' (b '<>' c) = (a '<>' b) '<>' c
-- @
--
--   Identity of '<>' (the 'Monoid' law, if @m@ is a 'Monoid'):
--
-- @
--   'zero' '<>' a    = a
--   a    '<>' 'zero' = a
-- @
--
--   Left-distributivity of '><<' over @m@ '<>':
--
-- @
--   r '><<' (x '<>' y) = r '><<' x '<>' r '><<' y
-- @
--
--   Left-distributivity of @r@ '<>' over '><<':
--
-- @
--   (r '<>' s) '><<' x = r '><<' x '<>' s '><<' x
-- @
--
--   Left-distributivity of '><' over '><<':
--
-- @
--   (r '><' s) '><<' x = r '><<' (s '><<' x)
-- @
--
--   Left-identity of '><<', if @r@ is 'Unital':
--
-- @
--   one ><< a = a
-- @
class (Semiring r, Semigroup m) => Module r m where
  infixl 7 ><<
  (><<) :: r -> m -> m


-- $
-- Associativity of '<>':
-- prop> a <> (b <> c) == (a <> b) <> (c :: ())
--
-- Identity of '<>':
-- prop> zero <> a == (a :: ())
-- prop> a <> zero == (a :: ())
--
-- Left-distributivity of '><<' over '<>':
-- prop> r ><< (x <> y) = r ><< x <> (r :: Boolean) ><< (y :: ())
--
-- Left-distributivity of '<>' over '><<':
-- prop> (r <> s) ><< x = r ><< x <> (s :: Boolean) ><< (x :: ())
--
-- Left-distributivity of '><' over '><<':
-- prop> (r >< s) ><< x = r ><< ((s :: Boolean) ><< (x :: ()))
--
-- Left-identity of '>><':
-- prop> (one :: Boolean) ><< a == (a :: ())
instance Semiring r => Module r () where
  _ ><< a = a


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Boolean
-- >>> import Data.Semiring.Class (Unital(..), zero)
