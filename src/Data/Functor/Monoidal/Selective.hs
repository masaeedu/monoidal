{-# LANGUAGE LambdaCase #-}
module Data.Functor.Monoidal.Selective where

import Prelude hiding (Applicative(..), zip)

import Control.Category ((>>>))
import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Applicative
import Data.Functor.Monoidal.Alternative
import Data.Functor.Monoidal.Decisive

import Data.Bifunctor
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

szipDAA :: (Decide f, Alt f, Apply f) => f (e + a) × f (e + b) -> f (e + a × b)
szipDAA = bimap decide decide >>> \case
  (Left e1, Left e2) -> Left <$> (e1 <|> e2)
  (Left e , Right _) -> Left <$> e
  (Right _, Left e ) -> Left <$> e
  (Right a, Right b) -> Right <$> zip a b

instance {-# OVERLAPPABLE #-}
  (Decide f, Alt f, Apply f) => Semigroupal (×) (×) (Tannen f (+) e)
  where
  combineF (Tannen a, Tannen b) = Tannen $ szipDAA (a, b)

instance {-# OVERLAPPABLE #-}
  (Semigroupal (×) (×) (Tannen f (+) e), Applicative f) => Monoidal (×) (×) (Tannen f (+) e)
  where
  unitF = Tannen . pure . Right
