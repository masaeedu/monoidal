module Data.Profunctor.Monoidal.Class where

import Data.Profunctor

import Control.Category.Tensor

class
  ( Associative l
  , Associative r
  , Associative o
  , Arrow l ~ (->)
  , Arrow r ~ (->)
  , Arrow o ~ (->)
  , Profunctor p
  ) =>
  Semigroupal l r o p
  where
  combineP :: p a b `o` p c d -> p (a `l` c) (b `r` d)

(-?-) :: Semigroupal l r (×) p => p a b -> p c d -> p (l a c) (r b d)
(-?-) = curry combineP

class
  ( Associative l
  , Associative r
  , Associative o
  , Arrow l ~ (->)
  , Arrow r ~ (->)
  , Arrow o ~ (->)
  , Profunctor p
  ) =>
  OpSemigroupal l r o p
  where
  uncombineP :: p (a `l` c) (b `r` d) -> p a b `o` p c d

type StrongSemigroupal l r o p = (Semigroupal l r o p, OpSemigroupal l r o p)

class
  ( Tensor l
  , Tensor r
  , Tensor o
  , Semigroupal l r o p
  ) =>
  Monoidal l r o p
  where
  unitP :: Unit o -> p (Unit l) (Unit r)

class
  ( Tensor l
  , Tensor r
  , Tensor o
  , OpSemigroupal l r o p
  ) =>
  OpMonoidal l r o p
  where
  discardP :: p (Unit l) (Unit r) -> Unit o

type StrongMonoidal l r o p = (Monoidal l r o p, OpMonoidal l r o p)

type Mux    = Semigroupal (×) (×) (×)
type Demux  = Semigroupal (+) (+) (×)
type Switch = Semigroupal (×) (+) (×)
type Splice = Semigroupal (+) (×) (×)

type Comux   = OpSemigroupal (×) (×) (×)
type Codemux = OpSemigroupal (+) (+) (×)

type Terminal  = Monoidal (×) (×) (×)
type Initial   = Monoidal (+) (+) (×)
type Universal = Monoidal (×) (+) (×)
type Unique    = Monoidal (+) (×) (×)
