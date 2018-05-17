module Data.Semiring.Class where

class Monoid r => Semiring r where
  infixr 7 ><
  (><) :: r -> r -> r
