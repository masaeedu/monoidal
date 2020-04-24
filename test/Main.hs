{-# LANGUAGE PartialTypeSignatures, ImpredicativeTypes #-}
module Main where

import Prelude hiding (zip, pure, (<*>))

import Test.Tasty

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

import Control.Category.Tensor

import Data.Bifunctor.Tannen

import Data.Functor.Monoidal.Class

import Data.Functor.Monoidal.Grid ()
import Data.Functor.Monoidal.Decisive ()
import Data.Functor.Monoidal.Filterable ()
import Data.Functor.Monoidal.Applicative
import Data.Functor.Monoidal.Alternative
import Data.Functor.Monoidal.Alignable
import Data.Functor.Monoidal.Selective ()

import Test.Monoidal

instance Semigroupal (⊠) (×) Gen
  where
  combineF = uncurry alignAA

instance Monoidal (⊠) (×) Gen
  where
  unitF = const empty

list' :: Gen x -> Gen [x]
list' = Gen.list (Range.linear 0 10)

slift :: (forall x. Gen x -> Gen (f x)) -> Gen a -> Gen (Tannen f (+) [Bool] a)
slift lift g = Tannen <$>
  (lift (Left <$> list' Gen.bool) <|> lift (Right <$> g))

maybeTests :: TestTree
maybeTests =
  let
    lift :: Gen x -> Gen (Maybe x)
    lift = Gen.maybe
  in
  testGroup "Maybe" $
  [ monoidal      @(×) @(×) "Applicative" lift Gen.bool
  , opsemigroupal @(+) @(+) "Decide"      lift Gen.bool
  , monoidal      @(+) @(×) "Alternative" lift Gen.bool
  , opmonoidal    @(+) @(×) "Filter"      lift Gen.bool
  , monoidal      @(⊠) @(×) "Align"       lift Gen.bool
  , monoidal      @(⊠) @(⊠) "Grid"        lift Gen.bool
  , monoidal      @(×) @(×) "Selective"   (slift lift) Gen.bool

  , distributive  @(×) @(×) @(+) @(×) "Applicative over Alternative" lift Gen.bool
  ]

listTests :: TestTree
listTests =
  let
    lift :: Gen x -> Gen [x]
    lift = Gen.list $ Range.linear 0 10
  in
  testGroup "[]" $
  [ monoidal      @(×) @(×) "Applicative" lift Gen.bool
  , opsemigroupal @(+) @(+) "Decide"      lift Gen.bool
  , monoidal      @(+) @(×) "Alternative" lift Gen.bool
  , opmonoidal    @(+) @(×) "Filter"      lift Gen.bool
  , monoidal      @(×) @(×) "Selective"   (slift lift) Gen.bool

  , rdistributive @(×) @(×) @(+) @(×) "Applicative over Alternative" lift Gen.bool
  ]

tupleTests :: TestTree
tupleTests =
  let
    lift :: Gen b -> Gen ([Bool], b)
    lift = ((,) <$> list' Gen.bool <*>)
  in
  testGroup "Tuple" $
  [ monoidal   @(×) @(×) "Applicative" lift Gen.bool
  , opmonoidal @(+) @(+) "Decisive"    lift Gen.bool
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ maybeTests
  , listTests
  , tupleTests
  ]
