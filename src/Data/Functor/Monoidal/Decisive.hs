module Data.Functor.Monoidal.Decisive where

import Control.Applicative (ZipList(..))
import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Strong.Class

import Data.Bifunctor
import Data.Void

decide :: Decide f => f (a + b) -> f a + f b
decide = uncombineF

refute :: Decisive f => f Void -> Void
refute = discardF @(+) @(+)

extract :: (Decisive f, OpStrong (+) (+) f) => f a -> a
extract = unembed @(+) @(+)

instance OpSemigroupal (+) (+) Maybe
  where
  uncombineF Nothing = Left Nothing
  uncombineF (Just (Left a)) = Left $ Just a
  uncombineF (Just (Right b)) = Right $ Just b

instance OpSemigroupal (+) (+) []
  where
  uncombineF [] = Left []
  uncombineF (Left a  : xs) = first  (a:) $ decide xs
  uncombineF (Right b : xs) = second (b:) $ decide xs

instance OpSemigroupal (+) (+) ((,) x)
  where
  uncombineF (x, Left a) = Left $ (x, a)
  uncombineF (x, Right b) = Right $ (x, b)

instance OpMonoidal (+) (+) ((,) a)
  where
  discardF = snd

consZipList :: a -> ZipList a -> ZipList a
consZipList a (ZipList xs) = ZipList $ a : xs

instance OpSemigroupal (+) (+) ZipList
  where
  uncombineF (ZipList []) = Left $ ZipList $ []
  uncombineF (ZipList (Left a  : xs)) = first  (a `consZipList`) $ decide (ZipList xs)
  uncombineF (ZipList (Right b : xs)) = second (b `consZipList`) $ decide (ZipList xs)
