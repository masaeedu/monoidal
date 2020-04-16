module Data.Functor.Monoidal.Filterable where

import Control.Category.Tensor
import Control.Category.Op

import Data.Functor.Monoidal.Class
import Data.Profunctor

partition :: Filter f => f (a + b) -> f a × f b
partition = dimap (fmap OpT) unOpT $ runOp $ combineF

mapMaybe :: Filter f => (a -> Maybe b) -> f a -> f b
mapMaybe ab = snd . partition . fmap (maybe (Left ()) Right . ab)

filter :: Filter f => (a -> Bool) -> f a -> f a
filter f = mapMaybe (\a -> if f a then Just a else Nothing)
