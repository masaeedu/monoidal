{-# LANGUAGE ImpredicativeTypes #-}
module Data.Functor.Monoidal.Class where

import Prelude hiding (Applicative(..))

import Control.Category.Iso
import Control.Category.Tensor
import Control.Category ((>>>), (<<<))

import Data.Bifunctor.Tannen

import Data.Functor.Identity
import Data.Functor.ComposeVia

import Data.Functor.Strong.Class

class
  ( Associative i
  , Associative o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  , Functor f
  ) =>
  Semigroupal i o f
  where
  combineF :: f a `o` f b -> f (a `i` b)

(-?-) :: Semigroupal t (×) f => f a -> f b -> f (t a b)
(-?-) = curry combineF

infixr -?-

class
  ( Associative i
  , Associative o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  , Functor f
  ) =>
  OpSemigroupal i o f
  where
  uncombineF :: f (a `i` b) -> f a `o` f b

type StrongSemigroupal i o f = (Semigroupal i o f, OpSemigroupal i o f)

class
  ( Tensor i
  , Tensor o
  , Semigroupal i o f
  ) =>
  Monoidal i o f
  where
  unitF :: Unit o -> f (Unit i)

husk :: forall t f. Monoidal t (×) f => f (Unit t)
husk = unitF @t @(×) ()

embed :: forall i o f a. (Monoidal i o f, Strong i o f) => a -> f a
embed =
  bwd runit
  >>> second (unitF @i @o)
  >>> lstrength @i @o
  >>> fmap (fwd runit)

unembed :: forall i o f a. (OpMonoidal i o f, OpStrong i o f) => f a -> a
unembed =
  fwd runit
  <<< second (discardF @i @o)
  <<< oplstrength @i @o
  <<< fmap (bwd runit)

class
  ( Tensor i
  , Tensor o
  , OpSemigroupal i o f
  ) =>
  OpMonoidal i o f
  where
  discardF :: f (Unit i) -> Unit o

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
  ( Associative t
  , Arrow t ~ (->)
  ) =>
  Semigroupal t t Identity
  where
  combineF = Identity . bimap runIdentity runIdentity

instance
  ( Tensor t
  , Arrow t ~ (->)
  ) =>
  Monoidal t t Identity
  where
  unitF = Identity

instance
  ( Associative t
  , Arrow t ~ (->)
  ) =>
  OpSemigroupal t t Identity
  where
  uncombineF = bimap Identity Identity . runIdentity

instance
  ( Tensor t
  , Arrow t ~ (->)
  ) =>
  OpMonoidal t t Identity
  where
  discardF = runIdentity

instance
  ( Semigroupal b c f
  , Semigroupal a b g
  ) =>
  Semigroupal a c (ComposeVia b f g)
  where
  combineF = ComposeVia . fmap combineF . combineF @b @c . bimap getComposeVia getComposeVia

instance
  ( Monoidal b c f
  , Monoidal a b g
  ) =>
  Monoidal a c (ComposeVia b f g)
  where
  unitF = ComposeVia . fmap (unitF @a @b) . unitF @b @c

instance
  ( OpSemigroupal b c f
  , OpSemigroupal a b g
  ) =>
  OpSemigroupal a c (ComposeVia b f g)
  where
  uncombineF = bimap ComposeVia ComposeVia . uncombineF @b @c . fmap uncombineF . getComposeVia

instance
  ( OpMonoidal b c f
  , OpMonoidal a b g
  ) =>
  OpMonoidal a c (ComposeVia b f g)
  where
  discardF = discardF @b @c . fmap (discardF @a @b) . getComposeVia
