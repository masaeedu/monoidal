{-# LANGUAGE ImpredicativeTypes #-}
module Data.Functor.Monoidal.Laws where

import Control.Category ((>>>), (<<<))
import Control.Category.Tensor
import Control.Category.Iso

import Data.Functor.Monoidal.Class

import Data.Bifunctor

associativity1, associativity2 :: forall t u f a b c.
  Semigroupal t u f =>
  f a `u` f b `u` f c -> f (a `t` (b `t` c))
associativity1 = fwd assoc >>> second combineF >>> combineF
associativity2 = first combineF >>> combineF >>> fmap (fwd assoc)

lunitality1, lunitality2 :: forall t u f a.
  Monoidal t u f =>
  Unit u `u` f a -> f a
lunitality1 = fwd lunit
lunitality2 = first (unitF @t @u @f) >>> combineF >>> fmap (fwd (lunit @t))

runitality1, runitality2 :: forall t u f a.
  Monoidal t u f =>
  f a `u` Unit u -> f a
runitality1 = fwd runit
runitality2 = second (unitF @t @u @f) >>> combineF >>> fmap (fwd (runit @t))

ldistributivity1, ldistributivity2 :: forall i1 o1 i2 o2 f a b c.
  ( Semigroupal i1 o1 f
  , Semigroupal i2 o2 f
  , LaxLDistrib i1 i2
  , LaxLDistrib o1 o2
  ) =>
  f a `o1` (f b `o2` f c) -> f ((a `i1` b) `i2` (a `i1` c))
ldistributivity1 = second combineF >>> combineF >>> fmap ldistrib
ldistributivity2 = ldistrib >>> bimap combineF combineF >>> combineF

rdistributivity1, rdistributivity2 :: forall i1 o1 i2 o2 f a b c.
  ( Semigroupal i1 o1 f
  , Semigroupal i2 o2 f
  , LaxRDistrib i1 i2
  , LaxRDistrib o1 o2
  ) =>
  (f a `o2` f b) `o1` f c -> f ((a `i1` c) `i2` (b `i1` c))
rdistributivity1 = first combineF >>> combineF >>> fmap rdistrib
rdistributivity2 = rdistrib >>> bimap combineF combineF >>> combineF

symmetry1, symmetry2 :: forall t u f a b.
  ( Semigroupal t u f
  , Symmetric t
  , Symmetric u
  ) =>
  f a `u` f b -> f (b `t` a)
symmetry1 = symm >>> combineF
symmetry2 = combineF >>> fmap symm

opassociativity1, opassociativity2 :: forall t u f a b c.
  OpSemigroupal t u f =>
  f (a `t` (b `t` c)) -> f a `u` f b `u` f c
opassociativity1 = bwd assoc <<< second uncombineF <<< uncombineF
opassociativity2 = first uncombineF <<< uncombineF <<< fmap (bwd assoc)

oplunitality1, oplunitality2 :: forall t u f a.
  OpMonoidal t u f =>
  f a -> Unit u `u` f a
oplunitality1 = bwd lunit
oplunitality2 = first (discardF @t @u @f) <<< uncombineF <<< fmap (bwd (lunit @t))

oprunitality1, oprunitality2 :: forall t u f a.
  OpMonoidal t u f =>
  f a -> f a `u` Unit u
oprunitality1 = bwd runit
oprunitality2 = second (discardF @t @u @f) <<< uncombineF <<< fmap (bwd (runit @t))

opldistributivity1, opldistributivity2 :: forall i1 o1 i2 o2 f a b c.
  ( OpSemigroupal i1 o1 f
  , OpSemigroupal i2 o2 f
  , OpLaxLDistrib i1 i2
  , OpLaxLDistrib o1 o2
  ) =>
  f ((a `i1` b) `i2` (a `i1` c)) -> f a `o1` (f b `o2` f c)
opldistributivity1 = second uncombineF <<< uncombineF <<< fmap opldistrib
opldistributivity2 = opldistrib <<< bimap uncombineF uncombineF <<< uncombineF

oprdistributivity1, oprdistributivity2 :: forall i1 o1 i2 o2 f a b c.
  ( OpSemigroupal i1 o1 f
  , OpSemigroupal i2 o2 f
  , OpLaxRDistrib i1 i2
  , OpLaxRDistrib o1 o2
  ) =>
  f ((a `i1` c) `i2` (b `i1` c)) -> (f a `o2` f b) `o1` f c
oprdistributivity1 = first uncombineF <<< uncombineF <<< fmap oprdistrib
oprdistributivity2 = oprdistrib <<< bimap uncombineF uncombineF <<< uncombineF

opsymmetry1, opsymmetry2 :: forall t u f a b.
  ( OpSemigroupal t u f
  , Symmetric t
  , Symmetric u
  ) =>
  f (b `t` a) -> f a `u` f b
opsymmetry1 = symm <<< uncombineF
opsymmetry2 = uncombineF <<< fmap symm
