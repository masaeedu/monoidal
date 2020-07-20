{-# LANGUAGE ImpredicativeTypes #-}
module Control.Category.Tensor.Test where

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import Hedgehog.Extra

import Control.Category.Tensor
import Control.Category.Tensor.Laws

import Data.Functor.Monoidal.Class

associative :: forall t a.
  ( Testable a
  , Testable2 t
  , Associative t
  , Semigroupal t (×) Gen
  ) =>
  String -> Gen a -> TestTree
associative name g =
  testGroup name $
  [ testProperty "Associativity: bwd . fwd = id" $ property $ do
      v <- forAll $ (g -?- g) -?- g
      assocfwdid1 @t v === assocfwdid2 @t v

  , testProperty "Associativity: fwd . bwd = id" $ property $ do
      v <- forAll $ g -?- g -?- g
      assocbwdid1 @t v === assocbwdid2 @t v

  , testProperty "Pentagon diagram" $ property $ do
      v <- forAll $ g -?- g -?- g -?- g
      pentagon1 @t v === pentagon2 @t v
  ]

unital :: forall t a.
  ( Testable a
  , Testable (Unit t)
  , Testable2 t
  , Unital t
  , Monoidal t (×) Gen
  ) =>
  String -> Gen a -> TestTree
unital name g =
  testGroup name $
  [ testProperty "Left unit: bwd . fwd = id" $ property $ do
      v <- forAll $ husk @t -?- g
      lunitfwdid1 @t v === lunitfwdid2 @t v

  , testProperty "Left unit: fwd . bwd = id" $ property $ do
      v <- forAll $ g
      lunitbwdid1 @t v === lunitbwdid2 @t v

  , testProperty "Right unit: bwd . fwd = id" $ property $ do
      v <- forAll $ g -?- husk @t
      runitfwdid1 @t v === runitfwdid2 @t v

  , testProperty "Right unit: fwd . bwd = id" $ property $ do
      v <- forAll $ g
      runitbwdid1 @t v === runitbwdid2 @t v

  , testProperty "Triangle diagram" $ property $ do
      v <- forAll $ g -?- (husk @t -?- g)
      triangle1 @t v === triangle2 @t v
  ]
