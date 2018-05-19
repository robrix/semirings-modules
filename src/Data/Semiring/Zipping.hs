{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Data.Semiring.Zipping where

import Control.Applicative (Alternative(..))
import Control.Monad.Fix (MonadFix(..))
import Data.Align (Align(..))
import Data.Data (Data(..))
import GHC.Generics (Generic, Generic1)

newtype Zipping a = Zipping { getZipping :: [a] }
  deriving (Data, Eq, Generic, Generic1, Ord, Read, Show)

instance Foldable Zipping where
  foldMap f = foldMap f . getZipping

instance Functor Zipping where
  fmap f = Zipping . fmap f . getZipping

instance Traversable Zipping where
  traverse f = fmap Zipping . traverse f . getZipping

instance Applicative Zipping where
  pure = Zipping . pure
  Zipping f <*> Zipping a = Zipping (zipWith ($) f a)

instance Align Zipping where
  nil = Zipping nil
  alignWith f (Zipping a) (Zipping b) = Zipping (alignWith f a b)

instance Alternative Zipping where
  empty = Zipping empty
  Zipping a <|> Zipping b = Zipping (go a b)
    where go [] bs = bs
          go as [] = as
          go (a:as) (_:bs) = a : go as bs

instance Monad Zipping where
  Zipping a >>= f = Zipping (a >>= getZipping . f)

instance MonadFix Zipping where
  mfix f = Zipping (mfix (getZipping . f))
