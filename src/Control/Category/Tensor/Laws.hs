{-# LANGUAGE ImpredicativeTypes, PartialTypeSignatures #-}
module Control.Category.Tensor.Laws where

import Prelude hiding (id)

import Control.Category
import Control.Category.Tensor
import Control.Category.Iso
import Control.Category.Iso.Laws

import Data.Bifunctor

assocfwdid1, assocfwdid2 ::
  Arrow t ~ p =>
  Associative t  =>
  (a `t` b `t` c) `p` (a `t` b `t` c)
assocfwdid1 = fwdid1 assoc
assocfwdid2 = fwdid2 assoc

assocbwdid1, assocbwdid2 ::
  Arrow t ~ p =>
  Associative t  =>
  (a `t` (b `t` c)) `p` (a `t` (b `t` c))
assocbwdid1 = bwdid1 assoc
assocbwdid2 = bwdid2 assoc

lunitfwdid1, lunitfwdid2 ::
  Arrow t ~ p =>
  Unit t ~ i =>
  Unital t =>
  (i `t` a) `p` (i `t` a)
lunitfwdid1 = fwdid1 lunit
lunitfwdid2 = fwdid2 lunit

lunitbwdid1, lunitbwdid2 :: forall t a p i.
  Arrow t ~ p =>
  Unit t ~ i =>
  Unital t =>
  a `p` a
lunitbwdid1 = bwdid1 (lunit @t)
lunitbwdid2 = bwdid2 (lunit @t)

runitfwdid1, runitfwdid2 ::
  Arrow t ~ p =>
  Unit t ~ i =>
  Unital t =>
  (a `t` i) `p` (a `t` i)
runitfwdid1 = fwdid1 runit
runitfwdid2 = fwdid2 runit

runitbwdid1, runitbwdid2 :: forall t a p i.
  Arrow t ~ p =>
  Unit t ~ i =>
  Unital t =>
  a `p` a
runitbwdid1 = bwdid1 (runit @t)
runitbwdid2 = bwdid2 (runit @t)

pentagon1, pentagon2 ::
  Arrow t ~ (->) =>
  Associative t =>
  a `t` (b `t` (c `t` d)) -> a `t` b `t` c `t` d
pentagon1 = bwd assoc >>> bwd assoc
pentagon2 = bimap id (bwd assoc) >>> bwd assoc >>> bimap (bwd assoc) id

triangle1, triangle2 ::
  Arrow t ~ (->) =>
  Unit t ~ i =>
  Tensor t =>
  a `t` (i `t` b) -> a `t` b
triangle1 = bimap id (fwd lunit)
triangle2 = bimap (fwd runit) id <<< bwd assoc
