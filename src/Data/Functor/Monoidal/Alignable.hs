module Data.Functor.Monoidal.Alignable where

import Prelude hiding (zip, (<*>))

import Control.Applicative (ZipList(..))

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

instance Semigroupal (⊠) (×) Maybe
  where
  combineF (Nothing, Nothing) = Nothing
  combineF (Just a, Nothing) = Just $ This a
  combineF (Nothing, Just b) = Just $ That b
  combineF (Just a, Just b) = Just $ These a b

instance Monoidal (⊠) (×) Maybe
  where
  unitF = const Nothing

instance Semigroupal (⊠) (×) []
  where
  combineF (xs, []) = This <$> xs
  combineF ([], ys) = That <$> ys
  combineF (x : xs, y : ys) = These x y : align xs ys

instance Monoidal (⊠) (×) []
  where
  unitF = const []

instance Semigroupal (⊠) (×) ZipList
  where
  combineF = uncurry alignAA

instance Monoidal (⊠) (×) ZipList
  where
  unitF = const $ ZipList $ []
