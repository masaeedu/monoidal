module Data.Functor.Monoidal.Alignable where

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.These

align :: Align f => f a -> f b -> f (These a b)
align = curry combineF

unalign :: Unalign f => f (These a b) -> f a × f b
unalign = uncombineF
