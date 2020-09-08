module Data.Biprofunctor.Downstream where

import Control.Category.Product
import Control.Category.Tensor.Hask

import Data.Biprofunctor
import Data.Biprofunctor.Monoidal.Class

import Data.Profunctor

newtype Downstream bp a b s t = Downstream { runDownstream :: bp a b s t }
  deriving (Biprofunctor, ProductCategory)

-- Monoidal biprofunctor stuff (you'd hope this was derivable 😞)
instance
  Semigroupal t1 t2 t3 t4 o bp =>
  Semigroupal t1 t2 t3 t4 o (Downstream bp)
  where
  combineBP = Downstream . combineBP . bimap runDownstream runDownstream

instance
  Monoidal t1 t2 t3 t4 o bp =>
  Monoidal t1 t2 t3 t4 o (Downstream bp)
  where
  unitBP = Downstream . unitBP @t1 @t2 @t3 @t4 @o

-- Profunctor stuff
instance Biprofunctor bp => Profunctor (Downstream bp a b)
  where
  dimap f g (Downstream p) = Downstream $ bidimap id id f g $ p
