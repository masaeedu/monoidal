module Data.Subtypes where

class Trivial x
instance Trivial x

class Impossible x

class c v => v <: c
instance c v => v <: c
