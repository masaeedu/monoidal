module Control.Category.Tensor.Hask
  ( module Control.Category.Tensor.Hask
  , module T'
  ) where

import Control.Category.Tensor as T'
  hiding
  ( Structure
  , Associative
  , Braided
  , Symmetric
  , Unital
  , Tensor
  , Bistructure
  , LaxLDistrib
  , LaxLAnnihil
  , LaxRDistrib
  , LaxRAnnihil
  , OpLaxLDistrib
  , OpLaxLAnnihil
  , OpLaxRDistrib
  , OpLaxRAnnihil
  , LRig
  , RRig
  , Rig
  )
import qualified Control.Category.Tensor as T

type OnHask        t = (Arrow t ~ (->), Ask t ~ NoThanks)

type Structure   t = (OnHask t, T.Structure t)
type Associative t = (OnHask t, T.Associative t)
type Braided     t = (OnHask t, T.Braided t)
type Symmetric   t = (OnHask t, T.Symmetric t)
type Unital      t = (OnHask t, T.Unital t)
type Tensor      t = (OnHask t, T.Tensor t)

type Bistructure   t1 t2 = (OnHask t1, OnHask t2, T.Bistructure t1 t2)
type LaxLDistrib   t1 t2 = (OnHask t1, OnHask t2, T.LaxLDistrib t1 t2)
type LaxLAnnihil   t1 t2 = (OnHask t1, OnHask t2, T.LaxLAnnihil t1 t2)
type LaxRDistrib   t1 t2 = (OnHask t1, OnHask t2, T.LaxRDistrib t1 t2)
type LaxRAnnihil   t1 t2 = (OnHask t1, OnHask t2, T.LaxRAnnihil t1 t2)
type OpLaxLDistrib t1 t2 = (OnHask t1, OnHask t2, T.OpLaxLDistrib t1 t2)
type OpLaxLAnnihil t1 t2 = (OnHask t1, OnHask t2, T.OpLaxLAnnihil t1 t2)
type OpLaxRDistrib t1 t2 = (OnHask t1, OnHask t2, T.OpLaxRDistrib t1 t2)
type OpLaxRAnnihil t1 t2 = (OnHask t1, OnHask t2, T.OpLaxRAnnihil t1 t2)
type LRig          t1 t2 = (OnHask t1, OnHask t2, T.LRig t1 t2)
type RRig          t1 t2 = (OnHask t1, OnHask t2, T.RRig t1 t2)
type Rig           t1 t2 = (OnHask t1, OnHask t2, T.Rig t1 t2)
