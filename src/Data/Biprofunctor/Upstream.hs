module Data.Biprofunctor.Upstream where

import Control.Category.ProductCategory
import Control.Category.Tensor.Hask

import Data.Biprofunctor
import Data.Biprofunctor.Monoidal.Class

import Data.Profunctor

newtype Upstream bp t s b a = Upstream { runUpstream :: bp a b s t }

instance Biprofunctor bp => Biprofunctor (Upstream bp)
  where
  bidimap f g h i (Upstream bp) = Upstream $ bidimap i h g f bp

instance ProductCategory bp => ProductCategory (Upstream bp)
  where
  biid = Upstream $ biid
  bicompose (Upstream f) (Upstream g) = Upstream $ bicompose g f

-- Monoidal biprofunctor stuff (you'd hope this was derivable 😞)
instance
  Semigroupal t4 t3 t2 t1 o bp =>
  Semigroupal t1 t2 t3 t4 o (Upstream bp)
  where
  combineBP = Upstream . combineBP . bimap runUpstream runUpstream

instance
  Monoidal t4 t3 t2 t1 o bp =>
  Monoidal t1 t2 t3 t4 o (Upstream bp)
  where
  unitBP = Upstream . unitBP @t4 @t3 @t2 @t1 @o

-- Profunctor stuff
instance Biprofunctor bp => Profunctor (Upstream bp t s)
  where
  dimap f g (Upstream p) = Upstream $ bidimap g f id id $ p
