module Data.Functor.Monoidal.Alignable where

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.These

align :: Align f => f a -> f b -> f (a ⊠ b)
align = curry combineF

unalign :: Unalign f => f (a ⊠ b) -> f a × f b
unalign = uncombineF
