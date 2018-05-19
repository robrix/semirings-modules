{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Module.Class where

import Data.Semiring.Class

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
--   Left-identity of '><<', if @r@ is 'Unital'.
class (Semiring r, Semigroup m) => Module r m where
  infixl 7 ><<
  (><<) :: r -> m -> m
