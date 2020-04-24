{-# LANGUAGE ImpredicativeTypes #-}
module Data.Functor.Monoidal.Laws where

import Control.Category ((>>>), (<<<))
import Control.Category.Tensor
import Control.Category.Iso
import Data.Functor.Monoidal.Class
import Data.Bifunctor

associativity1, associativity2 :: forall t u f a b c.
  Semigroupal t u f => f a `u` f b `u` f c -> f (a `t` (b `t` c))
associativity1 = fwd assoc >>> bimap id combineF >>> combineF
associativity2 = bimap combineF id >>> combineF >>> fmap (fwd assoc)

lunitality1, lunitality2 :: forall t u f a.
  Monoidal t u f =>
  Unit u `u` f a -> f a
lunitality1 = fwd lunit
lunitality2 = bimap (unitF @t @u @f) id >>> combineF >>> fmap (fwd (lunit @t))

runitality1, runitality2 :: forall t u f a.
  Monoidal t u f =>
  f a`u` Unit u -> f a
runitality1 = fwd runit
runitality2 = bimap id (unitF @t @u @f) >>> combineF >>> fmap (fwd (runit @t))

opassociativity1, opassociativity2 :: forall t u f a b c.
  OpSemigroupal t u f =>
  f (a `t` (b `t` c)) -> f a `u` f b `u` f c
opassociativity1 = bwd assoc <<< bimap id uncombineF <<< uncombineF
opassociativity2 = bimap uncombineF id <<< uncombineF <<< fmap (bwd assoc)

oplunitality1, oplunitality2 :: forall t u f a.
  OpMonoidal t u f =>
  f a -> Unit u `u` f a
oplunitality1 = bwd lunit
oplunitality2 = bimap (discardF @t @u @f) id <<< uncombineF <<< fmap (bwd (lunit @t))

oprunitality1, oprunitality2 :: forall t u f a.
  OpMonoidal t u f =>
  f a -> f a `u` Unit u
oprunitality1 = bwd runit
oprunitality2 = bimap id (discardF @t @u @f) <<< uncombineF <<< fmap (bwd (runit @t))

ldistributivity1, ldistributivity2 :: forall i1 i2 o1 o2 f a b c.
  (Semigroupal i1 o1 f, Semigroupal i2 o2 f, LaxLeftDistributive i1 i2, LaxLeftDistributive o1 o2) =>
  f a `o1` (f b `o2` f c) -> f ((a `i1` b) `i2` (a `i1` c))
ldistributivity1 = bimap id combineF >>> combineF >>> fmap ldistrib
ldistributivity2 = ldistrib >>> bimap combineF combineF >>> combineF

rdistributivity1, rdistributivity2 :: forall i1 i2 o1 o2 f a b c.
  (Semigroupal i1 o1 f, Semigroupal i2 o2 f, LaxRightDistributive i1 i2, LaxRightDistributive o1 o2) =>
  (f a `o2` f b) `o1` f c -> f ((a `i1` c) `i2` (b `i1` c))
rdistributivity1 = bimap combineF id >>> combineF >>> fmap rdistrib
rdistributivity2 = rdistrib >>> bimap combineF combineF >>> combineF
