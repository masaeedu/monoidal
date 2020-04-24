module Data.Functor.Monoidal.Alignable where

import Prelude hiding (zip, (<*>))

import Control.Category.Tensor
import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Applicative
import Data.Functor.Monoidal.Alternative
import Data.These

align :: Align f => f a -> f b -> f (a ⊠ b)
align = curry combineF

unalign :: Unalign f => f (a ⊠ b) -> f a × f b
unalign = uncombineF

alignAA :: (Apply f, Alt f) => f a -> f b -> f (a ⊠ b)
alignAA fa fb = These <$> fa <*> fb <|> This <$> fa <|> That <$> fb
