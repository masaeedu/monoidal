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

class Associative t => Tensor t
  where
  type Unit t :: *
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

instance Tensor (⊠)
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

instance LaxLeftDistributive (×) (+)
  where
  ldistrib (x, Left y) = Left (x, y)
  ldistrib (x, Right y) = Right (x, y)

instance LaxRightDistributive (×) (+)
  where
  rdistrib (Left x, y) = Left (x, y)
  rdistrib (Right x, y) = Right (x, y)

instance LaxLeftDistributive (×) (×)
  where
  ldistrib (x, (y, z)) = ((x, y), (x, z))

instance LaxRightDistributive (×) (×)
  where
  rdistrib ((x, y), z) = ((x, z), (y, z))
