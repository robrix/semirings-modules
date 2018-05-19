{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Module.Class where

import Data.Semiring.Class

class Semiring r => Module r m where
  (><<) :: r -> m -> m
