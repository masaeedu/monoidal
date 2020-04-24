{-# LANGUAGE TupleSections #-}
module Control.Category.Tensor where

import Prelude hiding ((.), id)

import Data.Bifunctor
import Data.Profunctor
import Data.Void
import Data.These
import Data.These.Combinators

import Control.Category
import Control.Category.Op
import Control.Category.Iso

type (×) = (,)
type (+) = Either
type (⊠) = These

infixl 6 +
infixl 7 ×

class (Category (Arrow t)) => Structure t
  where
  type Arrow t :: * -> * -> *

instance Structure (×)
  where
  type Arrow (×) = (->)

instance Structure (+)
  where
  type Arrow (+) = (->)

instance Structure (⊠)
  where
  type Arrow (⊠) = (->)

class (Structure t, Bifunctor t) => Associative t
  where
  assoc :: Iso (Arrow t) ((x `t` y) `t` z) (x `t` (y `t` z))

instance Associative (×)
  where
  assoc = Iso f b
    where
    f ((x, y), z) = (x, (y, z))
    b (x, (y, z)) = ((x, y), z)

instance Associative (+)
  where
  assoc = Iso f b
    where
    f = either (either Left (Right . Left)) (Right . Right)
    b = either (Left . Left) (either (Left . Right) Right)

instance Associative (⊠)
  where
  assoc = Iso assocThese unassocThese

class Structure t => Symmetric t
  where
  symm :: Arrow t (x `t` y) (y `t` x)

instance Symmetric (×)
  where
  symm (x, y) = (y, x)

instance Symmetric (+)
  where
  symm = either Right Left

instance Symmetric (⊠)
  where
  symm = swapThese

class Associative t => Tensor t
  where
  type Unit t = (r :: *) | r -> t
  lunit :: Iso (Arrow t) (t (Unit t) x) x
  runit :: Iso (Arrow t) (t x (Unit t)) x

instance Tensor (×)
  where
  type Unit (×) = ()
  lunit = Iso snd ((), )
  runit = Iso fst (, ())

instance Tensor (+)
  where
  type Unit (+) = Void
  lunit = Iso (either absurd id) Right
  runit = Iso (either id absurd) Left

newtype Neither = Neither { unNeither :: Void }

impossible :: Neither -> a
impossible = absurd . unNeither

instance Tensor (⊠)
  where
  type Unit (⊠) = Neither
  lunit = Iso (these impossible id impossible) That
  runit = Iso (these id impossible (const impossible)) This

newtype OpT t a b = OpT { unOpT :: t a b }
  deriving Bifunctor

newtype OpU i = OpU { unOpU :: i }

instance Structure t => Structure (OpT t)
  where
  type Arrow (OpT t) = Op (Arrow t)

instance (Profunctor (Arrow t), Bifunctor t, Associative t) => Associative (OpT t)
  where
  assoc = Iso f b
    where
    f = Op $ dimap (second unOpT . unOpT) (first OpT . OpT) $ bwd $ assoc @t
    b = Op $ dimap (first unOpT . unOpT) (second OpT . OpT) $ fwd (assoc @t)

instance (Profunctor (Arrow t), Bifunctor t, Tensor t) => Tensor (OpT t)
  where
  type Unit (OpT t) = OpU (Unit t)
  lunit = Iso f b
    where
    f = Op $ dimap id (OpT . first OpU) $ bwd $ lunit @t
    b = Op $ dimap (first unOpU . unOpT) id $ fwd $ lunit @t
  runit = Iso f b
    where
    f = Op $ dimap id (OpT . second OpU) $ bwd $ runit @t
    b = Op $ dimap (second unOpU . unOpT) id $ fwd $ runit @t
