module Data.Functor.Monoidal.Grid where

import Control.Category.Tensor
import Data.Functor.Monoidal.Class

import Data.These
import Data.Void

instance Semigroupal (⊠) (⊠) Maybe
  where
  combineF (This Nothing)            = Nothing
  combineF (That Nothing)            = Nothing
  combineF (This (Just a))           = Just $ This a
  combineF (That (Just b))           = Just $ That b
  combineF (These (Just a) Nothing)  = Just $ This a
  combineF (These Nothing (Just b))  = Just $ That b
  combineF (These Nothing Nothing)   = Nothing
  combineF (These (Just a) (Just b)) = Just $ These a b

instance Monoidal (⊠) (⊠) Maybe
  where
  unitF = absurd
