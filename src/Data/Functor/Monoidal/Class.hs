{-# LANGUAGE ImpredicativeTypes #-}
module Data.Functor.Monoidal.Class where

import Prelude hiding (Applicative(..))

import Control.Category.Tensor
import Control.Category.Op

import Data.Bifunctor.Tannen
import Data.Profunctor

class (Associative i, Associative o, Functor f) => Semigroupal i o f
  where
  combineF :: Arrow o (f a `o` f b) (f (a `i` b))

type OpSemigroupal i o = Semigroupal (OpT i) (OpT o)

uncombineF :: (Profunctor (Arrow o), OpSemigroupal i o f) => Arrow o (f (a `i` b)) (f a `o` f b)
uncombineF = dimap (fmap OpT) unOpT $ runOp $ combineF

type StrongSemigroupal i o f = (Semigroupal i o f, OpSemigroupal i o f)

class (Tensor i, Tensor o, Semigroupal i o f) => Monoidal i o f
  where
  unitF :: Arrow o (Unit o) (f (Unit i))

type OpMonoidal i o = Monoidal (OpT i) (OpT o)

discardF :: (Profunctor (Arrow o), OpMonoidal i o f) => Arrow o (f (Unit i)) (Unit o)
discardF = dimap (fmap OpU) unOpU $ runOp $ unitF

type StrongMonoidal i o f = (Monoidal i o f, OpMonoidal i o f)

type Apply   = Semigroupal   (×) (×)
type Decide  = OpSemigroupal (+) (+)
type Alt     = Semigroupal   (+) (×)
type Filter  = OpSemigroupal (+) (×)
type Align   = Semigroupal   (⊠) (×)
type Unalign = OpSemigroupal (⊠) (×)

type Applicative = Monoidal   (×) (×)
type Decisive    = OpMonoidal (+) (+)
type Alternative = Monoidal   (+) (×)

type Select f = (Functor f, forall a. Apply (Tannen f (+) a))
type Selective f = (Applicative f, Select f)

newtype FromBase f a = FromBase { unFromBase :: f a }
