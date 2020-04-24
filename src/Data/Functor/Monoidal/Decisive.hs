module Data.Functor.Monoidal.Decisive where

import Control.Category.Tensor

import Data.Functor.Monoidal.Class

import Data.Bifunctor
import Data.Void

decide :: Decide f => f (a + b) -> f a + f b
decide = uncombineF

refute :: Decisive f => f Void -> Void
refute = discardF @(+) @(+)

instance OpSemigroupal (+) (+) Maybe
  where
  uncombineF Nothing = Left Nothing
  uncombineF (Just (Left a)) = Left $ Just a
  uncombineF (Just (Right b)) = Right $ Just b

instance OpSemigroupal (+) (+) ((,) x)
  where
  uncombineF (x, Left a) = Left $ (x, a)
  uncombineF (x, Right b) = Right $ (x, b)

instance OpMonoidal (+) (+) ((,) a)
  where
  discardF = snd

instance OpSemigroupal (+) (+) []
  where
  uncombineF [] = Left []
  uncombineF (Left a : xs) = first (a:) $ decide xs
  uncombineF (Right b : xs) = second (b:) $ decide xs
