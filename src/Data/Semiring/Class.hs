module Data.Semiring.Class where

zero :: Monoid m => m
zero = mempty

class Monoid r => Semiring r where
  infixr 7 ><
  (><) :: r -> r -> r
