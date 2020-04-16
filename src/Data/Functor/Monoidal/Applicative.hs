module Data.Functor.Monoidal.Applicative where

import Prelude hiding (Applicative(..), zip)
import qualified Control.Applicative as A

import Control.Category.Tensor
import Data.Functor.Monoidal.Class

zip :: Apply f => f a -> f b -> f (a × b)
zip = curry combineF

pure :: Applicative f => a -> f a
pure a = a <$ unitF @(×) @(×) ()

liftA2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f fa fb = uncurry f <$> zip fa fb

(<*>) :: Apply f => f (a -> b) -> f a -> f b
(<*>) fab fa = (uncurry ($)) <$> zip fab fa

instance A.Applicative f => Semigroupal (×) (×) f
  where
  combineF (fa, fb) = (,) <$> fa A.<*> fb

instance A.Applicative f => Monoidal (×) (×) f
  where
  unitF = A.pure