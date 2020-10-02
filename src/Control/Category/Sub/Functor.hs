module Control.Category.Sub.Functor where

import Data.Subtypes
import Control.Category.Sub.Category (SubCat(..))

class (SubCat p, SubCat q, forall op oq x. (op ~ Ob p, oq ~ Ob q, x <: op) => f x <: oq) => GFunctor p q f
  where
  gfmap :: (Ob p a, Ob p b) => p a b -> q (f a) (f b)
