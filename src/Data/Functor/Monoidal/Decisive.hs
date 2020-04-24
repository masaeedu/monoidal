module Data.Functor.Monoidal.Decisive where

import Control.Category.Tensor

import Data.Functor.Monoidal.Class

import Data.Void

decide :: Decide f => f (a + b) -> f a + f b
decide = uncombineF

refute :: Decisive f => f Void -> Void
refute = discardF @(+) @(+)

