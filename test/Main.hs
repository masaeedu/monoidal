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

import qualified Test.Selective as SL

import Test.Monoidal

instance Semigroupal (⊠) (×) Gen
  where
  combineF = uncurry alignAA

instance Monoidal (⊠) (×) Gen
  where
  unitF = const empty

list' :: Gen x -> Gen [x]
list' = Gen.list (Range.linear 0 10)

slift :: (forall x. Gen x -> Gen (f x)) -> Gen a -> Gen (Tannen f (+) Bool a)
slift lift g = Tannen <$>
  (lift (Left <$> Gen.bool) <|> lift (Right <$> g))

maybeTests :: TestTree
maybeTests =
  let
    lift :: Gen x -> Gen (Maybe x)
    lift = Gen.maybe
  in
  testGroup "Maybe" $
  [ monoidal      @(×) @(×) "Applicative" lift Gen.bool
  , symmetric     @(×) @(×) "Applicative" lift Gen.bool
  , opsemigroupal @(+) @(+) "Decide"      lift Gen.bool
  , monoidal      @(+) @(×) "Alternative" lift Gen.bool
  , opmonoidal    @(+) @(×) "Filter"      lift Gen.bool
  , monoidal      @(⊠) @(×) "Align"       lift Gen.bool
  , symmetric     @(⊠) @(×) "Align"       lift Gen.bool
  , monoidal      @(⊠) @(⊠) "Grid"        lift Gen.bool
  , symmetric     @(⊠) @(×) "Grid"        lift Gen.bool
  , monoidal      @(×) @(×) "Selective"   (slift lift) Gen.bool

  , distributive   @(×) @(×) @(×) @(×) "Applicative over Applicative" lift Gen.bool
  , distributive   @(×) @(×) @(+) @(×) "Applicative over Alternative" lift Gen.bool
  , distributive   @(×) @(×) @(⊠) @(×) "Applicative over Align"       lift Gen.bool
  , opdistributive @(+) @(+) @(+) @(+) "Decisive    over Decisive"    lift Gen.bool
  , opdistributive @(+) @(×) @(+) @(+) "Filterable  over Decisive"    lift Gen.bool

  , SL.selective lift Gen.bool Gen.bool (pure id <|> pure not) (pure (&&) <|> pure (||))
  ]

listTests :: TestTree
listTests =
  let
    lift :: Gen x -> Gen [x]
    lift = list'
  in
  testGroup "[]" $
  [ monoidal      @(×) @(×) "Applicative" lift Gen.bool
  , opsemigroupal @(+) @(+) "Decide"      lift Gen.bool
  , monoidal      @(+) @(×) "Alternative" lift Gen.bool
  , opmonoidal    @(+) @(×) "Filter"      lift Gen.bool
  , monoidal      @(⊠) @(×) "Align"       lift Gen.bool
  , symmetric     @(⊠) @(×) "Align"       lift Gen.bool
  , monoidal      @(⊠) @(⊠) "Grid"        lift Gen.bool
  , monoidal      @(×) @(×) "Selective"   (slift lift) Gen.bool

  , rdistributive  @(×) @(×) @(+) @(×) "Applicative over Alternative" lift Gen.bool
  , rdistributive  @(×) @(×) @(⊠) @(×) "Applicative over Align"       lift Gen.bool
  , opdistributive @(+) @(+) @(+) @(+) "Decisive    over Decisive"    lift Gen.bool

  , SL.selective lift Gen.bool Gen.bool (pure id <|> pure not) (pure (&&) <|> pure (||))
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
