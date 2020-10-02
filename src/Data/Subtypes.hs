module Data.Subtypes where

import Data.Void (Void)

class Trivial x
instance Trivial x

class Impossible x
  where
  sorrynope :: Void

class c v => v <: c
instance c v => v <: c
