module Data.Biprofunctor.Upstream where

import Prelude hiding (id, (.))

import Control.Category
import Control.Category.Product
import Control.Category.Tensor.Hask
import Control.Category.Sub.Bifunctor

import Data.Biprofunctor
import Data.Biprofunctor.Monoidal.Class

import Data.Profunctor

import Data.Type.Equality
import Unsafe.Coerce

newtype Upstream bp t s b a = Upstream { runUpstream :: bp a b s t }

instance Biprofunctor bp => Biprofunctor (Upstream bp)
  where
  bidimap f g h i (Upstream bp) = Upstream $ bidimap i h g f bp

type family Swap (ab :: (a, b)) :: (b, a)
  where
  Swap '(a, b) = '(b, a)

swapPostulate1 :: Fst (Swap ab) :~: Snd ab
swapPostulate1 = unsafeCoerce Refl

swapPostulate2 :: Snd (Swap ab) :~: Fst ab
swapPostulate2 = unsafeCoerce Refl

instance ProductCategory bp => Category (Uncurry22 (Upstream bp))
  where
  id :: forall ab. Uncurry22 (Upstream bp) ab ab
  id = case (swapPostulate1 @ab, swapPostulate2 @ab) of
    (Refl, Refl) -> Uncurry22 $ Upstream $ runUncurry22 $ id @(Uncurry22 bp) @(Swap ab)

  (.) :: forall ab cd ef.
       Uncurry22 (Upstream bp) cd ef
    -> Uncurry22 (Upstream bp) ab cd
    -> Uncurry22 (Upstream bp) ab ef
  Uncurry22 (Upstream f) . Uncurry22 (Upstream g) = fg''
    where
    f' :: Uncurry22 bp (Swap ef) (Swap cd)
    f' = case (swapPostulate1 @cd, swapPostulate2 @cd, swapPostulate1 @ef, swapPostulate2 @ef) of
      (Refl, Refl, Refl, Refl) -> Uncurry22 f

    g' :: Uncurry22 bp (Swap cd) (Swap ab)
    g' = case (swapPostulate1 @ab, swapPostulate2 @ab, swapPostulate1 @cd, swapPostulate2 @cd) of
      (Refl, Refl, Refl, Refl) -> Uncurry22 g

    fg' :: Uncurry22 bp (Swap ef) (Swap ab)
    fg' = g' . f'

    fg'' :: Uncurry22 (Upstream bp) ab ef
    fg'' = case (swapPostulate1 @ab, swapPostulate2 @ab, swapPostulate1 @ef, swapPostulate2 @ef) of
      (Refl, Refl, Refl, Refl) -> Uncurry22 $ Upstream $ runUncurry22 $ fg'


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
