module Data.Profunctor.Monoidal.Demux where

import Control.Category.Tensor

import Data.Bifunctor
import Data.Bifunctor.Joker

import Data.Profunctor
import Data.Profunctor.Monoidal.Class

import Data.Void

import qualified Data.Functor.Monoidal.Class as F
import qualified Data.Functor.Monoidal.Decisive as F

demux :: Demux p => p a b -> p c d -> p (a + c) (b + d)
demux = curry combineP

initial :: Initial p => p Void a
initial = dimap id absurd $ unitP @(+) @(+) @(×) ()

fanin :: Demux p => p a x -> p b x -> p (a + b) x
fanin = (rmap (either id id) .) . demux

codemux :: Codemux p => p (a + b) (c + d) -> p a c × p b d
codemux = uncombineP

partitionCD :: Codemux p => p x (a + b) -> p x a × p x b
partitionCD = codemux . lmap (either id id)

instance Semigroupal (+) (+) (×) (->)
  where
  combineP = uncurry bimap

instance Functor f => Semigroupal (+) (+) (×) (Star f)
  where
  combineP (Star x, Star y) = Star $ either (fmap Left . x) (fmap Right . y)

instance Functor f => Monoidal (+) (+) (×) (Star f)
  where
  unitP _ = Star $ absurd

instance F.Alt f => Semigroupal (+) (+) (×) (Joker f)
  where
  combineP = Joker . F.combineF . bimap runJoker runJoker

instance F.Alternative f => Monoidal (+) (+) (×) (Joker f)
  where
  unitP = Joker . F.unitF @(+) @(×)

instance F.Decide f => Semigroupal (+) (+) (×) (Costar f)
  where
  combineP (Costar x, Costar y) = Costar $ bimap x y . F.decide

instance F.Decisive f => Monoidal (+) (+) (×) (Costar f)
  where
  unitP _ = Costar $ F.refute
