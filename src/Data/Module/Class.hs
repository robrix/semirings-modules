{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | R-modules over 'Semiring's.
module Data.Module.Class
(
-- * R-modules
  Module(..)
) where

import Data.Semiring.App (App(..))
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
-- Left-distributivity of '><<' over '<>':
--
-- prop> r ><< (x <> y) == r ><< x <> (r :: Boolean) ><< (y :: ())
--
-- Left-distributivity of '<>' over '><<':
--
-- prop> (r <> s) ><< x == r ><< x <> (s :: Boolean) ><< (x :: ())
--
-- Left-distributivity of '><' over '><<':
--
-- prop> (r >< s) ><< x == r ><< ((s :: Boolean) ><< (x :: ()))
--
-- Left-identity of '><<':
--
-- prop> (one :: Boolean) ><< a == (a :: ())
instance Semiring r => Module r () where
  _ ><< a = a

-- $
-- Left-distributivity of '><<' over '<>':
--
-- prop> r ><< (x <> y) == r ><< x <> (r :: Boolean) ><< (y :: Boolean)
--
-- Left-distributivity of '<>' over '><<':
--
-- prop> (r <> s) ><< x == r ><< x <> (s :: Boolean) ><< (x :: Boolean)
--
-- Left-distributivity of '><' over '><<':
--
-- prop> (r >< s) ><< x == r ><< ((s :: Boolean) ><< (x :: Boolean))
--
-- Left-identity of '><<':
--
-- prop> (one :: Boolean) ><< a == (a :: Boolean)
instance Semiring r => Module r r where
  (><<) = (><)

-- $
-- Left-distributivity of '><<' over '<>':
--
-- prop> r ><< (x <> y) == r ><< x <> (r :: Boolean) ><< (y :: (Boolean, Boolean))
--
-- Left-distributivity of '<>' over '><<':
--
-- prop> (r <> s) ><< x == r ><< x <> (s :: Boolean) ><< (x :: (Boolean, Boolean))
--
-- Left-distributivity of '><' over '><<':
--
-- prop> (r >< s) ><< x == r ><< ((s :: Boolean) ><< (x :: (Boolean, Boolean)))
--
-- Left-identity of '><<':
--
-- prop> (one :: Boolean) ><< a == (a :: (Boolean, Boolean))
instance Semiring r => Module r (r, r) where
  a ><< (b1, b2) = (a >< b1, a >< b2)

-- $
-- Left-distributivity of '><<' over '<>':
--
-- prop> r ><< (x <> y) == r ><< x <> (r :: Boolean) ><< (y :: (Boolean, Boolean, Boolean))
--
-- Left-distributivity of '<>' over '><<':
--
-- prop> (r <> s) ><< x == r ><< x <> (s :: Boolean) ><< (x :: (Boolean, Boolean, Boolean))
--
-- Left-distributivity of '><' over '><<':
--
-- prop> (r >< s) ><< x == r ><< ((s :: Boolean) ><< (x :: (Boolean, Boolean, Boolean)))
--
-- Left-identity of '><<':
--
-- prop> (one :: Boolean) ><< a == (a :: (Boolean, Boolean, Boolean))
instance Semiring r => Module r (r, r, r) where
  a ><< (b1, b2, b3) = (a >< b1, a >< b2, a >< b3)

-- $
-- Left-distributivity of '><<' over '<>':
--
-- prop> r ><< (x <> y) == r ><< x <> (r :: Boolean) ><< (y :: (Boolean, Boolean, Boolean, Boolean))
--
-- Left-distributivity of '<>' over '><<':
--
-- prop> (r <> s) ><< x == r ><< x <> (s :: Boolean) ><< (x :: (Boolean, Boolean, Boolean, Boolean))
--
-- Left-distributivity of '><' over '><<':
--
-- prop> (r >< s) ><< x == r ><< ((s :: Boolean) ><< (x :: (Boolean, Boolean, Boolean, Boolean)))
--
-- Left-identity of '><<':
--
-- prop> (one :: Boolean) ><< a == (a :: (Boolean, Boolean, Boolean, Boolean))
instance Semiring r => Module r (r, r, r, r) where
  a ><< (b1, b2, b3, b4) = (a >< b1, a >< b2, a >< b3, a >< b4)

-- $
-- Left-distributivity of '><<' over '<>':
--
-- prop> \ (Fn x) (Fn y) -> r ><< (x <> y) ~= r ><< x <> (r :: Boolean) ><< (y :: Int -> Boolean)
--
-- Left-distributivity of '<>' over '><<':
--
-- prop> \ (Fn x) -> (r <> s) ><< x ~= r ><< x <> (s :: Boolean) ><< (x :: Int -> Boolean)
--
-- Left-distributivity of '><' over '><<':
--
-- prop> \ (Fn x) -> (r >< s) ><< x ~= r ><< ((s :: Boolean) ><< (x :: Int -> Boolean))
--
-- Left-identity of '><<':
--
-- prop> \ (Fn a) -> (one :: Boolean) ><< a ~= (a :: Int -> Boolean)
instance Semiring r => Module r (a -> r) where
  (a ><< b) x = a >< b x

-- | Note that 'App'’s adherence to the distributivity laws depends on the behaviour of @f@. For example, 'App ZipList' is lawful, while 'App []' is not.
--
-- Left-distributivity of '><<' over '<>':
--
-- prop> r ><< (x <> y) == r ><< x <> (r :: Boolean) ><< (y :: App ZipList Boolean)
--
-- Left-distributivity of '<>' over '><<':
--
-- prop> (r <> s) ><< x == r ><< x <> (s :: Boolean) ><< (x :: App ZipList Boolean)
--
-- Left-distributivity of '><' over '><<':
--
-- prop> (r >< s) ><< x == r ><< ((s :: Boolean) ><< (x :: App ZipList Boolean))
--
-- Left-identity of '><<':
--
-- prop> (one :: Boolean) ><< a == (a :: App ZipList Boolean)
instance (Applicative f, Semiring r) => Module r (App f r) where
  a ><< App bs = App ((a ><) <$> bs)


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Test.QuickCheck.Function
-- >>> import Data.Semiring.Boolean
-- >>> import Data.Semiring.Class (Unital(..), zero)
-- >>> instance Arbitrary (f a) => Arbitrary (App f a) where arbitrary = App <$> arbitrary ; shrink (App f) = map App (shrink f)
-- >>> instance Arbitrary Boolean where arbitrary = Boolean <$> arbitrary ; shrink (Boolean b) = map Boolean (shrink b)
-- >>> :{
-- infix 4 ~=
-- f ~= g = (==) <$> f <*> g
-- :}
