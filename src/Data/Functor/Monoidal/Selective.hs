module Data.Functor.Monoidal.Selective where

import Prelude hiding ((<*>))
import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Applicative
import Data.Bifunctor.Tannen

flipTannen :: (Functor f, Symmetric t, Arrow t ~ (->)) => Tannen f t a b -> Tannen f t b a
flipTannen (Tannen t) = Tannen $ fmap symm $ t

liftTannen :: Functor f => f a -> Tannen f (+) x a
liftTannen = Tannen . fmap Right

extractTannen :: Functor f => Tannen f (+) a a -> f a
extractTannen = fmap (either id id) . runTannen

branch :: Select f => f (a -> r) -> f (b -> r) -> f (a + b) -> f r
branch fa fb = extractTannen . (liftTannen fa <*>) . flipTannen . (liftTannen fb <*>) . Tannen
