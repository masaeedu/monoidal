{-# LANGUAGE LambdaCase, TupleSections #-}
module Data.Functor.Monoidal.Selective where

import Prelude hiding (Applicative(..), zip)

import Control.Applicative (ZipList(..))
import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Applicative
import Data.Functor.Monoidal.Decisive

import Data.Bifunctor.Tannen

flipTannen :: (Functor f, Symmetric t, Arrow t ~ (->)) => Tannen f t a b -> Tannen f t b a
flipTannen (Tannen t) = Tannen $ fmap symm $ t

liftTannen :: Functor f => f a -> Tannen f (+) x a
liftTannen = Tannen . fmap Right

extractTannen :: Functor f => Tannen f (+) a a -> f a
extractTannen = fmap (either id id) . runTannen

szip :: Select f => f (e + a) × f (e + b) -> f (e + a × b)
szip = runTannen . combineF . bimap Tannen Tannen

branch :: Select f => f (a -> r) -> f (b -> r) -> f (a + b) -> f r
branch fa fb = extractTannen . (liftTannen fa <*>) . flipTannen . (liftTannen fb <*>) . Tannen

select :: Selective f => f (a + b) -> f (a -> b) -> f b
select v f = branch f (pure id) v

(<*?) :: Selective f => f (a + b) -> f (a -> b) -> f b
(<*?) = select

szipDAA :: (Decide f, Apply f) => f (e + a) × f (e + b) -> f (e + a × b)
szipDAA (x, y) = case decide x of
  Left e -> Left <$> e
  Right a -> fmap (first snd . ldistrib) $ zip a y

szipM :: Monad f => f (e + a) × f (e + b) -> f (e + a × b)
szipM (x, y) = x >>= either (return . Left) (\a -> fmap (fmap $ (a,)) y)

instance Semigroupal (×) (×) (Tannen Maybe (+) e)
  where
  combineF (Tannen a, Tannen b) = Tannen $ szipDAA (a, b)

instance Monoidal (×) (×) (Tannen Maybe (+) e)
  where
  unitF = Tannen . pure . Right

instance Semigroupal (×) (×) (Tannen [] (+) e)
  where
  combineF (Tannen a, Tannen b) = Tannen $ szipM (a, b)

instance Monoidal (×) (×) (Tannen [] (+) e)
  where
  unitF = Tannen . pure . Right

instance Semigroupal (×) (×) (Tannen ZipList (+) e)
  where
  combineF = Tannen . szipDAA . bimap runTannen runTannen

instance Monoidal (×) (×) (Tannen ZipList (+) e)
  where
  unitF = Tannen . pure . Right
