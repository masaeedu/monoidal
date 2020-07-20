{-# LANGUAGE ImpredicativeTypes #-}
module Hedgehog.Extra where

import GHC.Exts

type Lift1 c f = (forall x. c x => c (f x) :: Constraint)
type Lift2 c t = (forall x y. (c x, c y) => c (t x y) :: Constraint)

type Testable a = (Eq a, Show a)

type Testable1 f = (Lift1 Eq f, Lift1 Show f)

type Testable2 t = (Lift2 Eq t, Lift2 Show t)
