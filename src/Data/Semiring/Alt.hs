module Data.Semiring.Alt where

newtype Alt f a = Alt { getAlt :: f a }
