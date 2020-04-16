{-# LANGUAGE ImpredicativeTypes #-}
module Data.Functor.Monoidal.Class where

import Prelude hiding (Applicative(..))
import Control.Category.Tensor

import Data.Bifunctor.Tannen

class (Associative i, Associative o, Functor f) => Semigroupal i o f
  where
  combineF :: Arrow o (f a `o` f b) (f (a `i` b))

class (Tensor i, Tensor o, Semigroupal i o f) => Monoidal i o f
  where
  unitF :: Arrow o (Unit o) (f (Unit i))

type Apply  = Semigroupal (×) (×)
type Decide = Semigroupal (OpT (+)) (OpT (+))
type Alt    = Semigroupal (+) (×)
type Filter = Semigroupal (OpT (+)) (OpT (×))

type Applicative = Monoidal (×) (×)
type Decisive    = Monoidal (OpT (+)) (OpT (+))
type Alternative = Monoidal (+) (×)

type Select f = (Functor f, forall a. Apply (Tannen f (+) a))
type Selective f = (Applicative f, Select f)

newtype FromBase f a = FromBase { unFromBase :: f a }
