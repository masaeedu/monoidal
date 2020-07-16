module Data.Profunctor.Monoidal.Class where

import Data.Profunctor (Profunctor(..))
import Data.Profunctor.Strong.Class
import Data.Profunctor.ProcomposeVia

import Control.Category.Iso
import Control.Category.Tensor

import Data.Bifunctor

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

diagonal :: forall l r p. Monoidal l r (×) p => p (Unit l) (Unit r)
diagonal = unitP @l @r @(×) ()

identity :: forall l r p a. (Monoidal l r (×) p, Strong l r p) => p a a
identity = dimap (bwd $ lunit @l) (fwd $ lunit @r) $ lstrength $ diagonal @l @r

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
type Trivial = OpSemigroupal (+) (×) (×)

-- Every profunctor has instances of the trivial semigroupal and monoidal constraints
instance Profunctor p => OpSemigroupal (+) (×) (×) p
  where
  uncombineP p = (dimap Left fst p, dimap Right snd p)

instance Profunctor p => OpMonoidal (+) (×) (×) p
  where
  discardP = const ()

type Terminal  = Monoidal (×) (×) (×)
type Initial   = Monoidal (+) (+) (×)
type Universal = Monoidal (×) (+) (×)
type Unique    = Monoidal (+) (×) (×)

instance
  ( Associative t
  , Arrow t ~ (->)
  ) =>
  Semigroupal t t (×) (->)
  where
  combineP = uncurry bimap

instance
  ( Tensor t
  , Arrow t ~ (->)
  ) =>
  Monoidal t t (×) (->)
  where
  unitP = const id

instance
  ( Semigroupal x b (×) p
  , Semigroupal a x (×) q
  ) =>
  Semigroupal a b (×) (ProcomposeVia x p q)
  where
  combineP (ProcomposeVia a1 b1, ProcomposeVia a2 b2) =
    ProcomposeVia
      (combineP @x @b $ (a1, a2))
      (combineP @a @x $ (b1, b2))

instance
  ( Monoidal x b (×) p
  , Monoidal a x (×) q
  ) =>
  Monoidal a b (×) (ProcomposeVia x p q)
  where
  unitP = const $ ProcomposeVia (diagonal @x @b) (diagonal @a @x)
