{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Data.Semiring.Mult where

newtype Mult r = Mult { getMult :: r }
  deriving (Enum, Eq, Foldable, Functor, Num, Ord, Read, Show, Traversable)
