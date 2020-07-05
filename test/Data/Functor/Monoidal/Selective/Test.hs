{-# LANGUAGE ImpredicativeTypes #-}
module Data.Functor.Monoidal.Selective.Test where

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog hiding (Eq1, Show1)
import Hedgehog.Extra

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Selective.Laws
import Data.Functor.Monoidal.Alternative ()
import Data.Functor.Monoidal.Selective ()

selective :: forall f e a.
  ( Selective f
  , Testable1 f
  , Testable e
  , Testable a
  , Testable2 (+)
  ) =>
  (forall x. Gen x -> Gen (f x)) ->
  Gen e ->
  Gen a ->
  Gen (e -> a) ->
  Gen (e -> e -> a) ->
  TestTree
selective lift e a f f2 =
  testGroup "Selective" $
  [ testProperty "Identity" $ property $ do
      v <- forAll $ lift $ e -?- e
      identity1 v === identity2 v

  , testProperty "Distributivity" $ property $ do
      v <- forAllWith (const "whatever") $ (e -?- a) -?- lift f -?- lift f
      distributivity1 v === distributivity2 v

  , testProperty "Associativity" $ property $ do
      v <- forAllWith (const "whatever") $ lift (e -?- a) -?- lift (e -?- f) -?- lift f2
      associativity1 @f @e @e @a v === associativity2 v
  ]
