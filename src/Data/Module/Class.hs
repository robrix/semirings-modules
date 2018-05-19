{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Left & right R-modules over 'Semiring's.
module Data.Module.Class
(
-- * Left R-modules
  LeftModule(..)
-- * Right R-modules
, RightModule(..)
) where

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
class (Semiring r, Semigroup m) => LeftModule r m where
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
instance Semiring r => LeftModule r () where
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
instance Semiring r => LeftModule r r where
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
instance Semiring r => LeftModule r (r, r) where
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
instance Semiring r => LeftModule r (r, r, r) where
  a ><< (b1, b2, b3) = (a >< b1, a >< b2, a >< b3)


-- | A right @r@-module over a 'Semiring' @r@.
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
--   Right-distributivity of '>><' over @m@ '<>':
--
-- @
--   (x '<>' y) '>><' r = x '>><' r '<>' y '>><' r
-- @
--
--   Right-distributivity of @r@ '<>' over '>><':
--
-- @
--   x '>><' (r '<>' s) = x '>><' r '<>' x '>><' s
-- @
--
--   Right-distributivity of '><' over '>><':
--
-- @
--   x '>><' (r '><' s) = (r '>><' x) '>><' s
-- @
--
--   Right-identity of '>><', if @r@ is 'Unital':
--
-- @
--   a >>< one = a
-- @
class (Semiring r, Semigroup m) => RightModule r m where
  infixl 7 >><
  (>><) :: m -> r -> m

-- $
-- Right-distributivity of '>><' over '<>':
--
-- prop> r >>< (x <> y) == r >>< x <> (r :: Boolean) >>< (y :: ())
--
-- Right-distributivity of '<>' over '>><':
--
-- prop> (r <> s) >>< x == r >>< x <> (s :: Boolean) >>< (x :: ())
--
-- Right-distributivity of '><' over '>><':
--
-- prop> (r >< s) >>< x == r >>< ((s :: Boolean) >>< (x :: ()))
--
-- Right-identity of '>><':
--
-- prop> (one :: Boolean) >>< a == (a :: ())
instance Semiring r => RightModule r () where
  a >>< _ = a

-- $
-- Right-distributivity of '>><' over '<>':
--
-- prop> r >>< (x <> y) == r >>< x <> (r :: Boolean) >>< (y :: Boolean)
--
-- Right-distributivity of '<>' over '>><':
--
-- prop> (r <> s) >>< x == r >>< x <> (s :: Boolean) >>< (x :: Boolean)
--
-- Right-distributivity of '><' over '>><':
--
-- prop> (r >< s) >>< x == r >>< ((s :: Boolean) >>< (x :: Boolean))
--
-- Right-identity of '>><':
--
-- prop> (one :: Boolean) >>< a == (a :: Boolean)
instance Semiring r => RightModule r r where
  (>><) = (><)

-- $
-- Right-distributivity of '>><' over '<>':
--
-- prop> r >>< (x <> y) == r >>< x <> (r :: Boolean) >>< (y :: (Boolean, Boolean))
--
-- Right-distributivity of '<>' over '>><':
--
-- prop> (r <> s) >>< x == r >>< x <> (s :: Boolean) >>< (x :: (Boolean, Boolean))
--
-- Right-distributivity of '><' over '>><':
--
-- prop> (r >< s) >>< x == r >>< ((s :: Boolean) >>< (x :: (Boolean, Boolean)))
--
-- Right-identity of '>><':
--
-- prop> (one :: Boolean) >>< a == (a :: (Boolean, Boolean))
instance Semiring r => RightModule r (r, r) where
  (a1, a2) >>< b = (a1 >< b, a2 >< b)


-- $setup
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> import Data.Semiring.Boolean
-- >>> import Data.Semiring.Class (Unital(..), zero)
-- >>> instance Arbitrary Boolean where arbitrary = Boolean <$> arbitrary ; shrink (Boolean b) = map Boolean (shrink b)
