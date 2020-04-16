module Data.Functor.Monoidal.Decisive where

import Control.Category.Tensor
import Control.Category.Op

import Data.Functor.Monoidal.Class
import Data.Profunctor
import Data.Void

decide :: Decide f => f (a + b) -> f a + f b
decide = dimap (fmap OpT) unOpT $ runOp $ combineF

refute :: Decisive f => f Void -> Void
refute = dimap (fmap OpU) unOpU $ runOp $ unitF
