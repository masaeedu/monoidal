{-# LANGUAGE ImpredicativeTypes #-}
module Data.Functor.Monoidal.Class where

import Prelude hiding (Applicative(..))

import Control.Category.Tensor

import Data.Bifunctor
import Data.Bifunctor.Tannen

import Data.Functor.Compose

class
  ( Associative i
  , Associative o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  , Functor f
  ) =>
  Semigroupal i o f
  where
  combineF :: Arrow o (f a `o` f b) (f (a `i` b))

class
  ( Associative i
  , Associative o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  , Functor f
  ) =>
  OpSemigroupal i o f
  where
  uncombineF :: Arrow o (f (a `i` b)) (f a `o` f b)

type StrongSemigroupal i o f = (Semigroupal i o f, OpSemigroupal i o f)

class
  ( Tensor i
  , Tensor o
  , Semigroupal i o f
  ) =>
  Monoidal i o f
  where
  unitF :: Arrow o (Unit o) (f (Unit i))

class
  ( Tensor i
  , Tensor o
  , OpSemigroupal i o f
  ) =>
  OpMonoidal i o f
  where
  discardF :: Arrow o (f (Unit i)) (Unit o)

type StrongMonoidal i o f = (Monoidal i o f, OpMonoidal i o f)

type Apply   = Semigroupal   (×) (×)
type Decide  = OpSemigroupal (+) (+)
type Alt     = Semigroupal   (+) (×)
type Filter  = OpSemigroupal (+) (×)
type Align   = Semigroupal   (⊠) (×)
type Unalign = OpSemigroupal (⊠) (×)
type Grid    = Semigroupal   (⊠) (⊠)

type Applicative = Monoidal   (×) (×)
type Decisive    = OpMonoidal (+) (+)
type Alternative = Monoidal   (+) (×)

type Select f = (Functor f, forall a. Apply (Tannen f (+) a))
type Selective f = (Applicative f, Select f)

instance
  ( Semigroupal b c f
  , Semigroupal a b g
  ) => Semigroupal a c (Compose f g)
  where
  combineF = Compose . fmap combineF . combineF @b @c . bimap getCompose getCompose
