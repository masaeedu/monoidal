module Data.Biprofunctor.Downstream where

import Prelude hiding (id, (.))

import Control.Category
import Control.Category.Product
import Control.Category.Tensor.Hask
import Control.Category.Sub.Bifunctor

import Data.Biprofunctor
import Data.Biprofunctor.Monoidal.Class

import Data.Profunctor

import Data.Coerce

newtype Downstream bp a b s t = Downstream { runDownstream :: bp a b s t }
  deriving (Biprofunctor)

instance Category (Uncurry22 bp) => Category (Uncurry22 (Downstream bp))
  where
  id :: forall ab. Uncurry22 (Downstream bp) ab ab
  id = coerce $ id @(Uncurry22 bp) @ab

  (.) :: forall ab cd ef.
       Uncurry22 (Downstream bp) cd ef
    -> Uncurry22 (Downstream bp) ab cd
    -> Uncurry22 (Downstream bp) ab ef
  (.) = coerce $ (.) @(Uncurry22 bp) @cd @ef @ab

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
