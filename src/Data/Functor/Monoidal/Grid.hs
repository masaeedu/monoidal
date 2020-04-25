module Data.Functor.Monoidal.Grid where

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Alignable

import Data.These
import Data.Void

grid :: Grid f => f a ⊠ f b -> f (a ⊠ b)
grid = combineF

gridA :: Align f => f a ⊠ f b -> f (a ⊠ b)
gridA (This x) = This <$> x
gridA (That x) = That <$> x
gridA (These x y) = align x y

instance Semigroupal (⊠) (⊠) f => Monoidal (⊠) (⊠) f
  where
  unitF = absurd

instance Semigroupal (⊠) (⊠) Maybe
  where
  combineF = gridA

instance Semigroupal (⊠) (⊠) []
  where
  combineF = gridA
