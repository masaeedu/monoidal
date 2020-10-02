module Control.Category.Product where

import Prelude hiding (id, (.))

import GHC.Exts (Constraint)

import Control.Category (Category(..))
import Control.Category.Sub.Category (SubCat(..))
import Data.Type.Equality (type (:~:)(..))

import Unsafe.Coerce (unsafeCoerce)

data BiArrow (p :: k -> k -> *) (q :: l -> l -> *) (ab :: (k, l)) (cd :: (k, l)) :: *
  where
  BiArrow :: p a b -> q c d -> BiArrow p q '(a, c) '(b, d)

type family Fst (ab :: (x, y)) :: x
  where
  Fst '(a, _) = a

type family Snd (ab :: (x, y)) :: y
  where
  Snd '(_, b) = b

-- We need to postulate this because this is not a given in Haskell's kind system
-- Near as I can figure out this is because of non-totality (i.e. you can make up type families with codomain of kind (a, b))
tuplePostulate :: ab :~: '(Fst ab, Snd ab)
tuplePostulate = unsafeCoerce Refl

instance (Category p, Category q) => Category (BiArrow p q)
  where
  id :: forall ab. BiArrow p q ab ab
  id = case tuplePostulate @ab of Refl -> BiArrow id id

  BiArrow f g . BiArrow h i = BiArrow (f . h) (g . i)

class (f (Fst ab), g (Snd ab)) => Both (f :: k -> Constraint) (g :: l -> Constraint) (ab :: (k, l))
instance (f (Fst ab), g (Snd ab)) => Both f g ab

instance (SubCat p, SubCat q) => SubCat (BiArrow p q)
  where
  type Ob (BiArrow p q) = Both (Ob p) (Ob q)

newtype Uncurry22 :: (a -> b -> c -> d -> *) -> (a, b) -> (c, d) -> *
  where
  Uncurry22 :: { runUncurry22 :: p (Fst ab) (Snd ab) (Fst cd) (Snd cd) } -> Uncurry22 p ab cd

type ProductCategory p = Category (Uncurry22 p)

biid :: forall p ab. ProductCategory p => p (Fst ab) (Snd ab) (Fst ab) (Snd ab)
biid = runUncurry22 $ id @(Uncurry22 p) @ab

bicompose :: forall p ab cd ef. ProductCategory p
  => p (Fst cd) (Snd cd) (Fst ef) (Snd ef)
  -> p (Fst ab) (Snd ab) (Fst cd) (Snd cd)
  -> p (Fst ab) (Snd ab) (Fst ef) (Snd ef)
p `bicompose` q = runUncurry22 $ pq'
  where
  p' :: Uncurry22 p cd ef
  p' = Uncurry22 p

  q' :: Uncurry22 p ab cd
  q' = Uncurry22 q

  pq' :: Uncurry22 p ab ef
  pq' = p' . q'
