{-# LANGUAGE TupleSections, InstanceSigs #-}
module Control.Category.Tensor
  ( module Control.Category.Tensor
  , module Data.Subtypes
  , module Control.Category.Sub
  ) where

import Prelude hiding ((.), id)

import Data.Subtypes (type (<:), Impossible, Trivial)
import qualified Data.Bifunctor as B
import Data.Void (absurd, Void)
import Data.These (these, These(This, That))
import Data.These.Combinators (assocThese, swapThese, unassocThese)

import Control.Category (Category(..))
import Control.Category.Iso (Iso(..))
import Control.Category.Sub (SubCat(..), GFunctor(..), GBifunctor(..), bimap, first, second)
import Control.Category.Product (BiArrow(..))
import Control.Category.Uncurry (Uncurry0(..))

import Data.Coerce (coerce)

type (×) = (,)
type (+) = Either
type (⊠) = These

infixr 6 +
infixr 7 ×

-- {{{ TENSOR CLASSES

type Ask t = Ob (Arrow t)

class
  ( GBifunctor (Arrow t) t
  ) =>
  Structure (t :: k -> k -> k)
  where
  type Arrow t :: k -> k -> *

class Structure t => Associative t
  where
  assoc :: (Ask t x, Ask t y, Ask t z) => Iso (Arrow t) ((x `t` y) `t` z) (x `t` (y `t` z))

class Structure t => Braided t
  where
  braid :: (Ask t x, Ask t y) => Iso (Arrow t) (x `t` y) (y `t` x)

  default braid :: (Ask t x, Ask t y, Symmetric t) => Iso (Arrow t) (x `t` y) (y `t` x)
  braid = Iso symm symm

class Braided t => Symmetric t
  where
  symm :: (Ask t x, Ask t y) => Arrow t (x `t` y) (y `t` x)

class (Structure t, Ask t (Unit t)) => Unital (t :: k -> k -> k)
  where
  type Unit t :: k
  lunit' :: Ask t x => Iso (Arrow t) (t (Unit t) x) x
  runit' :: Ask t x => Iso (Arrow t) (t x (Unit t)) x

lunit :: forall t x. (Unital t, Ask t x) => Iso (Arrow t) (t (Unit t) x) x
lunit = lunit'

runit :: forall t x. (Unital t, Ask t x) => Iso (Arrow t) (t x (Unit t)) x
runit = runit'

type Tensor t = (Associative t, Unital t)

type Bistructure t u = (Structure t, Structure u, Arrow t ~ Arrow u, Ask t ~ Ask u)

class Bistructure times plus => LaxLDistrib times plus
  where
  ldistrib :: Arrow times (x `times` (y `plus` z)) ((x `times` y) `plus` (x `times` z))

class Bistructure times plus => LaxLAnnihil times plus
  where
  lnihil :: Arrow times (x `times` Unit plus) (Unit plus)

class Bistructure times plus => LaxRDistrib times plus
  where
  rdistrib :: Arrow times ((x `plus` y) `times` z) ((x `times` z) `plus` (y `times` z))

class Bistructure times plus => LaxRAnnihil times plus
  where
  rnihil :: Arrow times (Unit plus `times` x) (Unit plus)

class Bistructure times plus => OpLaxLDistrib times plus
  where
  opldistrib :: Arrow times ((x `times` y) `plus` (x `times` z)) (x `times` (y `plus` z))

class Bistructure times plus => OpLaxLAnnihil times plus
  where
  oplnihil :: Arrow times (Unit plus) (x `times` Unit plus)

class Bistructure times plus => OpLaxRDistrib times plus
  where
  oprdistrib :: Arrow times ((x `times` z) `plus` (y `times` z)) ((x `plus` y) `times` z)

class Bistructure times plus => OpLaxRAnnihil times plus
  where
  oprnihil :: Arrow times (Unit plus) (Unit plus `times` x)

type LRig times plus =
  ( LaxLDistrib times plus
  , LaxLAnnihil times plus
  , OpLaxLDistrib times plus
  , OpLaxLAnnihil times plus
  )

type RRig times plus =
  ( LaxRDistrib times plus
  , LaxRAnnihil times plus
  , OpLaxRDistrib times plus
  , OpLaxRAnnihil times plus
  )

type Rig times plus = (LRig times plus, RRig times plus)

-- }}}

-- {{{ STRUCTURE

instance B.Bifunctor t => Structure t
  where
  type Arrow t = (->)

-- }}}

-- {{{ ASSOCIATIVE

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

-- }}}

-- {{{ BRAIDED, SYMMETRIC

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

-- }}}

-- {{{ UNITAL

instance Unital (×)
  where
  type Unit (×) = ()
  lunit' = Iso snd ((), )
  runit' = Iso fst (, ())

instance Unital (+)
  where
  type Unit (+) = Void
  lunit' = Iso (either absurd id) Right
  runit' = Iso (either id absurd) Left

instance Unital (⊠)
  where
  type Unit (⊠) = Void
  lunit' = Iso (these absurd id absurd) That
  runit' = Iso (these id absurd (const absurd)) This

-- }}}

-- {{{ DISTRIBUTIVE

-- (×) apparently distributes laxly over any monoidal structure. This fact is probably specific to Hask, since I'm using something like the tensorial strengths of a Bifunctor wrt (×), which are trivial in Hask, but not in an arbitrary category.
instance
  Bistructure (×) plus =>
  LaxLDistrib (×) plus
  where
  ldistrib (x, p) = bimap (x, ) (x, ) p

instance
  Bistructure (×) plus =>
  LaxLAnnihil (×) plus
  where
  lnihil = snd

instance
  Bistructure (×) plus =>
  LaxRDistrib (×) plus
  where
  rdistrib (p, z) = bimap (, z) (, z) p

instance
  Bistructure (×) plus =>
  LaxRAnnihil (×) plus
  where
  rnihil = fst

-- Any monoidal structure distributes over the cartesian product (from ncatlab). This distributivity probably isn't super useful, but we'll still witness it.
instance {-# INCOHERENT #-}
  Bistructure times (×) =>
  LaxLDistrib times (×)
  where
  ldistrib v = (second fst v, second snd v)

instance {-# INCOHERENT #-}
  Bistructure times (×) =>
  LaxLAnnihil times (×)
  where
  lnihil = const ()

instance {-# INCOHERENT #-}
  Bistructure times (×) =>
  LaxRDistrib times (×)
  where
  rdistrib v = (first fst v, first snd v)

instance {-# INCOHERENT #-}
  Bistructure times (×) =>
  LaxRAnnihil times (×)
  where
  rnihil = const ()

-- Dually, any monoidal structure distributes oplaxly over the cocartesian monoidal structure (which is the cartesian monoidal structure in the opposite category).
instance
  Bistructure times (+) =>
  OpLaxLDistrib times (+)
  where
  opldistrib = either (second Left) (second Right)

instance
  Bistructure times (+) =>
  OpLaxLAnnihil times (+)
  where
  oplnihil = absurd

instance
  Bistructure times (+) =>
  OpLaxRDistrib times (+)
  where
  oprdistrib = either (first Left) (first Right)

instance
  Bistructure times (+) =>
  OpLaxRAnnihil times (+)
  where
  oprnihil = absurd

-- }}}
