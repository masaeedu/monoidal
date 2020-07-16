{-# LANGUAGE TupleSections #-}
module Data.Functor.Strong.Class where

import Control.Category ((>>>))
import Control.Category.Tensor

import Data.Functor.Identity
import Data.Functor.Compose

import Data.Bifunctor

class
  ( Functor f
  , Tensor i
  , Tensor o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  ) =>
  LStrong i o f
  where
  lstrength :: o a (f b) -> f (i a b)

class
  ( Functor f
  , Tensor i
  , Tensor o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  ) =>
  RStrong i o f
  where
  rstrength :: o (f a) b -> f (i a b)

type Strong i o f = (LStrong i o f, RStrong i o f)

class
  ( Functor f
  , Tensor i
  , Tensor o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  ) =>
  OpLStrong i o f
  where
  oplstrength :: f (i a b) -> o a (f b)

class
  ( Functor f
  , Tensor i
  , Tensor o
  , Arrow i ~ (->)
  , Arrow o ~ (->)
  ) =>
  OpRStrong i o f
  where
  oprstrength :: f (i a b) -> o (f a) b

type OpStrong i o f = (OpLStrong i o f, OpRStrong i o f)

instance {-# INCOHERENT #-} Functor f => LStrong (×) (×) f
  where
  lstrength (a, fb) = fmap (a,) fb

instance {-# INCOHERENT #-} Functor f => RStrong (×) (×) f
  where
  rstrength (fa, b) = fmap (,b) fa

instance
  ( Tensor i
  , Arrow i ~ (->)
  ) =>
  LStrong i i Identity
  where
  lstrength =
    second runIdentity
    >>> Identity

instance
  ( Tensor i
  , Arrow i ~ (->)
  ) =>
  RStrong i i Identity
  where
  rstrength =
    first runIdentity
    >>> Identity

instance
  ( LStrong x o f
  , LStrong i x g
  ) =>
  LStrong i o (Compose f g)
  where
  lstrength =
    second getCompose
    >>> lstrength @x @o
    >>> fmap lstrength
    >>> Compose

instance
  ( RStrong x o f
  , RStrong i x g
  ) =>
  RStrong i o (Compose f g)
  where
  rstrength =
    first getCompose
    >>> rstrength @x @o
    >>> fmap rstrength
    >>> Compose
