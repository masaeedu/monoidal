module Control.Category.Sub.Endofunctor where

import Control.Category.Sub.Category (SubCat(..))
import Control.Category.Sub.Functor (GFunctor(..))

type GEndofunctor p f = GFunctor p p f

fmap :: (GEndofunctor p f, Ob p a, Ob p b) => p a b -> p (f a) (f b)
fmap = gfmap

instance Functor f => GFunctor (->) (->) f
  where
  gfmap = Prelude.fmap
