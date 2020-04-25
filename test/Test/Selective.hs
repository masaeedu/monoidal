{-# LANGUAGE ImpredicativeTypes #-}
module Test.Selective where

import GHC.Exts

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog hiding (Eq1, Show1)

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Selective.Laws
import Data.Functor.Monoidal.Alternative ()
import Data.Functor.Monoidal.Selective ()

type Testable a = (Eq a, Show a)

type Eq1 f = (forall x. Eq x => Eq (f x) :: Constraint)
type Show1 f = (forall x. Show x => Show (f x) :: Constraint)
type Testable1 f = (Eq1 f, Show1 f)

type Eq2 t = (forall x y. (Eq x, Eq y) => Eq (t x y) :: Constraint)
type Show2 t = (forall x y. (Show x, Show y) => Show (t x y) :: Constraint)
type Testable2 t = (Eq2 t, Show2 t)

selective :: forall f e a.
  ( Selective f
  , Testable1 f
  , Testable e
  , Testable a
  , Testable2 (+)
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen e -> Gen a -> Gen (e -> a) -> Gen (e -> e -> a) -> TestTree
selective lift e a f f2 =
  testGroup "Selective" $
  [ testProperty "Identity" $ property $ do
      v <- forAll $ lift $ combineF (e, e)
      identity1 v === identity2 v

  , testProperty "Distributivity" $ property $ do
      v <- forAllWith (const "whatever") $ combineF (combineF (e, a), combineF (lift f, lift f))
      distributivity1 v === distributivity2 v

  , testProperty "Associativity" $ property $ do
      v <- forAllWith (const "whatever") $ combineF (lift (combineF (e, a)), combineF (lift $ combineF (e, f), lift f2))
      associativity1 @f @e @e @a v === associativity2 v
  ]
