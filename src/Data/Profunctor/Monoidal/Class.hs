module Data.Profunctor.Monoidal.Class where

import Data.Profunctor

import Control.Category.Tensor

class (Associative l, Associative r, Associative o, Profunctor p) => Semigroupal l r o p
  where
  combineP :: Arrow o (p a b `o` p c d) (p (a `l` c) (b `r` d))

class (Tensor l, Tensor r, Tensor o, Semigroupal l r o p) => Monoidal l r o p
  where
  unitP :: Arrow o (Unit o) (p (Unit l) (Unit r))

type Mux    = Semigroupal (×) (×) (×)
type Demux  = Semigroupal (+) (+) (×)
type Switch = Semigroupal (×) (+) (×)
type Splice = Semigroupal (+) (×) (×)

type Terminal  = Monoidal (×) (×) (×)
type Initial   = Monoidal (+) (+) (×)
type Universal = Monoidal (×) (+) (×)
type Unique    = Monoidal (+) (×) (×)

newtype FromBase p a b = FromBase { unFromBase :: p a b }
