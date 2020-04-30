{-# LANGUAGE TupleSections #-}
module Control.Category.Tensor where

import Prelude hiding ((.), id)

import Data.Bifunctor
import Data.Void
import Data.These
import Data.These.Combinators

import Control.Category
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

class Structure t => Braided t
  where
  braid :: Iso (Arrow t) (x `t` y) (y `t` x)

  default braid :: Symmetric t => Iso (Arrow t) (x `t` y) (y `t` x)
  braid = Iso symm symm

class Braided t => Symmetric t
  where
  symm :: Arrow t (x `t` y) (y `t` x)

instance Braided (×)
instance Symmetric (×)
  where
  symm (x, y) = (y, x)

instance Braided (+)
instance Symmetric (+)
  where
  symm = either Right Left

instance Braided (⊠)
instance Symmetric (⊠)
  where
  symm = swapThese

class Structure t => Unital t
  where
  type Unit t :: *
  lunit :: Iso (Arrow t) (t (Unit t) x) x
  runit :: Iso (Arrow t) (t x (Unit t)) x

type Tensor t = (Associative t, Unital t)

instance Unital (×)
  where
  type Unit (×) = ()
  lunit = Iso snd ((), )
  runit = Iso fst (, ())

instance Unital (+)
  where
  type Unit (+) = Void
  lunit = Iso (either absurd id) Right
  runit = Iso (either id absurd) Left

instance Unital (⊠)
  where
  type Unit (⊠) = Void
  lunit = Iso (these absurd id absurd) That
  runit = Iso (these id absurd (const absurd)) This

class (Structure times, Structure plus, Arrow times ~ Arrow plus) => LaxLeftDistributive times plus
  where
  ldistrib :: Arrow times (x `times` (y `plus` z)) ((x `times` y) `plus` (x `times` z))

class (Structure times, Structure plus, Arrow times ~ Arrow plus) => LaxRightDistributive times plus
  where
  rdistrib :: Arrow times ((x `plus` y) `times` z) ((x `times` z) `plus` (y `times` z))

class OpLaxLeftDistributive times plus
  where
  opldistrib :: Arrow times ((x `times` y) `plus` (x `times` z)) (x `times` (y `plus` z))

class OpLaxRightDistributive times plus
  where
  oprdistrib :: Arrow times ((x `times` z) `plus` (y `times` z)) ((x `plus` y) `times` z)

type LeftRig  times plus = (LaxLeftDistributive  times plus, OpLaxLeftDistributive  times plus)
type RightRig times plus = (LaxRightDistributive times plus, OpLaxRightDistributive times plus)

type Rig times plus = (LeftRig times plus, RightRig times plus)

instance
  ( Structure plus
  , Bifunctor plus
  , Arrow plus ~ Arrow (×)
  ) =>
  LaxLeftDistributive (×) plus
  where
  ldistrib (x, p) = bimap (x, ) (x, ) p

instance
  ( Structure plus
  , Bifunctor plus
  , Arrow plus ~ Arrow (×)
  ) =>
  LaxRightDistributive (×) plus
  where
  rdistrib (p, z) = bimap (, z) (, z) p

instance
  ( Structure times
  , Bifunctor times
  , Arrow times ~ Arrow (+)
  ) =>
  OpLaxLeftDistributive times (+)
  where
  opldistrib = either (second Left) (second Right)

instance
  ( Structure times
  , Bifunctor times
  , Arrow times ~ Arrow (+)
  ) =>
  OpLaxRightDistributive times (+)
  where
  oprdistrib = either (first Left) (first Right)
