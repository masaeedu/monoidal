module Data.Functor.Monoidal.Filterable where

import Control.Applicative (ZipList(..))

import Control.Category.Tensor

import Data.Functor.Monoidal.Class

import Data.Bifunctor

partition :: Filter f => f (a + b) -> f a × f b
partition = uncombineF

mapMaybe :: Filter f => (a -> Maybe b) -> f a -> f b
mapMaybe ab = snd . partition . fmap (maybe (Left ()) Right . ab)

filter :: Filter f => (a -> Bool) -> f a -> f a
filter f = mapMaybe (\a -> if f a then Just a else Nothing)

instance OpSemigroupal (+) (×) f => OpMonoidal (+) (×) f
  where
  discardF = const ()

instance OpSemigroupal (+) (×) Maybe
  where
  uncombineF Nothing = (Nothing, Nothing)
  uncombineF (Just (Left a)) = (Just a, Nothing)
  uncombineF (Just (Right b)) = (Nothing, Just b)

-- TODO: Figure out the connection between this and the
-- similar looking Decide instance for []
instance OpSemigroupal (+) (×) []
  where
  uncombineF [] = ([], [])
  uncombineF (Left a : xs) = first (a:) $ partition xs
  uncombineF (Right b : xs) = second (b:) $ partition xs

deriving via [] instance OpSemigroupal (+) (×) ZipList
