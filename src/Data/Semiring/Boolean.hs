module Data.Semiring.Boolean
(
-- * Boolean algebras
  Boolean(..)
) where

-- | Boolean algebras form a semiring with '<>' as disjunction, '><' as conjunction, 'zero' as 'False', and 'one' as 'True'.
newtype Boolean = Boolean { getBoolean :: Bool }
