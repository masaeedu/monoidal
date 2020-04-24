module Data.Functor.Monoidal.Filterable where

import Control.Category.Tensor

import Data.Functor.Monoidal.Class

partition :: Filter f => f (a + b) -> f a × f b
partition = uncombineF

mapMaybe :: Filter f => (a -> Maybe b) -> f a -> f b
mapMaybe ab = snd . partition . fmap (maybe (Left ()) Right . ab)

filter :: Filter f => (a -> Bool) -> f a -> f a
filter f = mapMaybe (\a -> if f a then Just a else Nothing)

instance OpSemigroupal (+) (×) f => OpMonoidal (+) (×) f
  where
  discardF = const ()
