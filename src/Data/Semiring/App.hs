module Data.Semiring.App where

newtype App f a = App { getApp :: f a }
