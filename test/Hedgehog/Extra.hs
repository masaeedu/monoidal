{-# LANGUAGE ImpredicativeTypes #-}
module Hedgehog.Extra where

import GHC.Exts

type Testable a = (Eq a, Show a)

type Eq1 f = (forall x. Eq x => Eq (f x) :: Constraint)
type Show1 f = (forall x. Show x => Show (f x) :: Constraint)
type Testable1 f = (Eq1 f, Show1 f)

type Eq2 t = (forall x y. (Eq x, Eq y) => Eq (t x y) :: Constraint)
type Show2 t = (forall x y. (Show x, Show y) => Show (t x y) :: Constraint)
type Testable2 t = (Eq2 t, Show2 t)
