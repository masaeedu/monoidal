module Control.Category.Sub.Nat where

import Prelude hiding (id, (.))

import Control.Category
import Control.Category.Sub.Category
import Control.Category.Sub.Functor

newtype NatOf :: (i -> i -> *) -> (o -> o -> *) -> (i -> o) -> (i -> o) -> *
  where
  NatOf :: { runNatOf :: forall x. q (f x) (g x) } -> NatOf p q f g

instance (Category p, Category q) => Category (NatOf p q)
  where
  id = NatOf id
  NatOf f . NatOf g = NatOf $ f . g

instance (SubCat p, SubCat q) => SubCat (NatOf p q)
  where
  type Ob (NatOf p q) = GFunctor p q
