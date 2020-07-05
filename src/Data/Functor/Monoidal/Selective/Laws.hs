{-# LANGUAGE PartialTypeSignatures, TupleSections #-}
module Data.Functor.Monoidal.Selective.Laws where

import Prelude hiding (Applicative(..), (*>))
import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Applicative
import Data.Functor.Monoidal.Selective

import Data.Bifunctor

identity1, identity2 :: forall f e.
  Selective f =>
  f (e + e) -> f e
identity1 = (<*? pure id)
identity2 = fmap (either id id)

distributivity1, distributivity2 :: forall f e a.
  Selective f =>
  (e + a) × f (e -> a) × f (e -> a) -> f a
distributivity1 (x, (y, z)) = pure x <*? (y *> z)
distributivity2 (x, (y, z)) = (pure x <*? y) *> (pure x <*? z)

associativity1, associativity2 :: forall f e1 e2 a.
  Selective f =>
  f (e1 + a) × f (e2 + (e1 -> a)) × f (e2 -> e1 -> a) -> f a
associativity1 (x, (y, z)) = x <*? (y <*? z)
associativity2 (x, (y, z)) = (f <$> x) <*? (g <$> y) <*? (h <$> z)
  where
  f = fmap Right
  g y a = bimap (,a) ($a) y
  h = uncurry
