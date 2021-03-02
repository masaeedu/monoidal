module Control.Category.Sub.Bifunctor where

import Prelude hiding (id, (.))

import Control.Category (Category(..))
import Control.Category.Sub.Category (SubCat(..))
import Control.Category.Product (BiArrow(..))
import Control.Category.Sub.Functor (GFunctor(..))
import Control.Category.Uncurry (Uncurry0(..))

import qualified Data.Bifunctor as B

import Data.Subtypes (type (<:))

import Data.Coerce (coerce)

class
  ( GFunctor (BiArrow p p) p (Uncurry t)
  , forall ob x y. (ob ~ Ob p, x <: ob, y <: ob) => t x y <: ob
  ) =>
  GBifunctor (p :: k -> k -> *) (t :: k -> k -> k)
  where
  type Uncurry t :: (k, k) -> k

  uncurryB :: p (t a b) (Uncurry t '(a, b))
  curryB :: p (Uncurry t '(a, b)) (t a b)

bimap :: (GBifunctor p t, Ob p a, Ob p b, Ob p c, Ob p d) => p a b -> p c d -> p (t a c) (t b d)
bimap f g = curryB . gfmap (BiArrow f g) . uncurryB

first :: (GBifunctor p t, Ob p a, Ob p b, Ob p x) => p a b -> p (t a x) (t b x)
first = flip bimap id

second :: (GBifunctor p t, Ob p a, Ob p b, Ob p x) => p a b -> p (t x a) (t x b)
second = bimap id

instance B.Bifunctor t => GFunctor (BiArrow (->) (->)) (->) (Uncurry0 t)
  where
  gfmap (BiArrow f g) = Uncurry0 . B.bimap f g . runUncurry0

instance B.Bifunctor t => GBifunctor (->) t
  where
  type Uncurry t = Uncurry0 t
  uncurryB = coerce
  curryB = coerce
