module Data.Semiring.Boolean
(
-- * Boolean algebras
  Boolean(..)
) where

newtype Boolean = Boolean { getBoolean :: Bool }
