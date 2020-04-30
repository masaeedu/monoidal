module Data.Profunctor.Monoidal.Mux where

import Control.Category.Tensor hiding (Arrow)
import Control.Arrow

import Data.Bifunctor
import Data.Bifunctor.Joker

import Data.Profunctor
import Data.Profunctor.Monoidal.Class

import qualified Data.Functor.Monoidal.Class as F

mux :: Mux p => p a b -> p c d -> p (a × c) (b × d)
mux = curry combineP

terminal :: Terminal p => p a ()
terminal = dimap (const ()) id $ unitP @(×) @(×) @(×) ()

zipM :: Mux p => p x a -> p x b -> p x (a × b)
zipM = (lmap (\x -> (x, x)) .) . mux

comux :: Comux p => p (a × b) (c × d) -> p a c × p b d
comux = uncombineP

undivideCM :: Comux p => p (a × b) x -> p a x × p b x
undivideCM = comux . rmap (\x -> (x, x))

muxA :: Arrow p => p a b × p c d -> p (a × c) (b × d)
muxA = uncurry (***)

instance Semigroupal (×) (×) (×) (->)
  where
  combineP = muxA

instance F.Apply f => Semigroupal (×) (×) (×) (Star f)
  where
  combineP (Star x, Star y) = Star $ F.combineF . bimap x y

instance F.Applicative f => Monoidal (×) (×) (×) (Star f)
  where
  unitP _ = Star $ F.unitF @(×) @(×)

instance F.Apply f => Semigroupal (×) (×) (×) (Joker f)
  where
  combineP (Joker x, Joker y) = Joker $ F.combineF (x, y)

instance F.Applicative f => Monoidal (×) (×) (×) (Joker f)
  where
  unitP = Joker . F.unitF @(×) @(×)

instance Functor f => Semigroupal (×) (×) (×) (Costar f)
  where
  combineP (Costar x, Costar y) = Costar $ \v -> (x $ fmap fst $ v, y $ fmap snd $ v)

instance Functor f => Monoidal (×) (×) (×) (Costar f)
  where
  unitP _ = Costar $ const ()
