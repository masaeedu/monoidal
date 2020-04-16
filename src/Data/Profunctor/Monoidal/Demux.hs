module Data.Profunctor.Monoidal.Demux where

import Control.Category.Op
import Control.Category.Tensor
import Data.Profunctor.Monoidal.Class
import Data.Profunctor
import Data.Void

demux :: Demux p => p a b -> p c d -> p (a + c) (b + d)
demux = curry combineP

initial :: Initial p => p Void a
initial = dimap id absurd $ unitP ()

fanin :: Demux p => p a x -> p b x -> p (a + b) x
fanin = (rmap (either id id) .) . demux

codemux :: Codemux p => p (a + b) (c + d) -> p a c × p b d
codemux = unOpT . runOp combineP

partition :: Codemux p => p x (a + b) -> p x a × p x b
partition = codemux . lmap (either id id)
