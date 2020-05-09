{-# LANGUAGE PartialTypeSignatures, ImpredicativeTypes #-}
module Main where

import Prelude hiding (zip, pure, (<*>))

import Test.Tasty

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

import Control.Category.Tensor

import Control.Applicative (ZipList(..))

import Data.Bifunctor.Tannen

import Data.Functor.Monoidal.Class

import Data.Functor.Monoidal.Grid ()
import Data.Functor.Monoidal.Decisive ()
import Data.Functor.Monoidal.Filterable ()
import Data.Functor.Monoidal.Applicative
import Data.Functor.Monoidal.Alternative
import Data.Functor.Monoidal.Alignable
import Data.Functor.Monoidal.Selective ()

import qualified Data.Functor.Monoidal.Selective.Test as SL
import Data.Functor.Monoidal.Test
import Control.Category.Tensor.Test

instance Semigroupal (+) (×) Gen
  where
  combineF (g1, g2) = Gen.choice [Left <$> g1, Right <$> g2]

instance Semigroupal (⊠) (×) Gen
  where
  combineF = uncurry alignAA

instance Monoidal (⊠) (×) Gen
  where
  unitF = const empty

int' :: Gen Integer
int' = Gen.integral $ Range.linear  0 10

list' :: Gen x -> Gen [x]
list' = Gen.list (Range.linear 0 10)

slift :: (forall x. Gen x -> Gen (f x)) -> Gen a -> Gen (Tannen f (+) Integer a)
slift lift g = Tannen <$>
  (lift (Left <$> int') <|> lift (Right <$> g))

timesTests :: TestTree
timesTests =
  testGroup "×" $
  [ associative @(×) "×" int'
  , unital      @(×) "×" int'
  ]

sumTests :: TestTree
sumTests =
  testGroup "+" $
  [ associative @(+) "+" int'
  , unital      @(+) "+" int'
  ]

theseTests :: TestTree
theseTests =
  testGroup "⊠" $
  [ associative @(+) "⊠" int'
  , unital      @(+) "⊠" int'
  ]

maybeTests :: TestTree
maybeTests =
  let
    lift :: Gen x -> Gen (Maybe x)
    lift = Gen.maybe
  in
  testGroup "Maybe" $
  [ monoidal      @(×) @(×) "Applicative" lift int'
  , symmetric     @(×) @(×) "Applicative" lift int'
  , opsemigroupal @(+) @(+) "Decide"      lift int'
  , monoidal      @(+) @(×) "Alternative" lift int'
  , opmonoidal    @(+) @(×) "Filter"      lift int'
  , opsymmetric   @(+) @(×) "Filter"      lift int'
  , monoidal      @(⊠) @(×) "Align"       lift int'
  , symmetric     @(⊠) @(×) "Align"       lift int'
  , monoidal      @(⊠) @(⊠) "Grid"        lift int'
  , symmetric     @(⊠) @(×) "Grid"        lift int'
  , monoidal      @(×) @(×) "Selective"   (slift lift) int'

  , distributive   @(×) @(×) @(×) @(×) "Applicative over Applicative" lift int'
  , distributive   @(×) @(×) @(+) @(×) "Applicative over Alternative" lift int'
  , distributive   @(×) @(×) @(⊠) @(×) "Applicative over Align"       lift int'
  , opdistributive @(+) @(+) @(+) @(+) "Decisive    over Decisive"    lift int'

  , SL.selective lift int' int' (pure id <|> pure negate) (pure (*) <|> pure (+))
  ]

listTests :: TestTree
listTests =
  let
    lift :: Gen x -> Gen [x]
    lift = list'
  in
  testGroup "[]" $
  [ monoidal      @(×) @(×) "Applicative" lift int'
  , opsemigroupal @(+) @(+) "Decide"      lift int'
  , monoidal      @(+) @(×) "Alternative" lift int'
  , opmonoidal    @(+) @(×) "Filter"      lift int'
  , monoidal      @(⊠) @(×) "Align"       lift int'
  , symmetric     @(⊠) @(×) "Align"       lift int'
  , monoidal      @(⊠) @(⊠) "Grid"        lift int'
  , symmetric     @(⊠) @(⊠) "Grid"        lift int'
  , monoidal      @(×) @(×) "Selective"   (slift lift) int'

  , rdistributive  @(×) @(×) @(+) @(×) "Applicative over Alternative" lift int'
  , rdistributive  @(×) @(×) @(⊠) @(×) "Applicative over Align"       lift int'
  , opdistributive @(+) @(+) @(+) @(+) "Decisive    over Decisive"    lift int'

  , SL.selective lift int' int' (pure id <|> pure negate) (pure (*) <|> pure (+))
  ]

zlTests :: TestTree
zlTests =
  let
    lift :: Gen x -> Gen (ZipList x)
    lift = fmap ZipList . list'
  in
  testGroup "ZipList" $
  [ monoidal      @(×) @(×) "Applicative" lift int'
  , symmetric     @(×) @(×) "Applicative" lift int'
  , opsemigroupal @(+) @(+) "Decide"      lift int'
  , opsymmetric   @(+) @(+) "Decide"      lift int'
  , monoidal      @(+) @(×) "Alternative" lift int'
  , symmetric     @(+) @(×) "Alternative" lift int'
  , opmonoidal    @(+) @(×) "Filter"      lift int'
  , opsymmetric   @(+) @(×) "Filter"      lift int'
  , monoidal      @(⊠) @(×) "Align"       lift int'
  , symmetric     @(⊠) @(×) "Align"       lift int'
  , monoidal      @(×) @(×) "Selective"   (slift lift) int'
  , symmetric     @(×) @(×) "Selective"   (slift lift) int'

  , distributive   @(×) @(×) @(×) @(×) "Applicative over Applicative" lift int'
  , distributive   @(×) @(×) @(⊠) @(×) "Applicative over Align"       lift int'
  , distributive   @(×) @(×) @(+) @(×) "Applicative over Alternative" lift int'
  , opdistributive @(+) @(+) @(+) @(+) "Decisive    over Decisive"    lift int'
  , opdistributive @(+) @(×) @(+) @(+) "Filterable  over Decisive"    lift int'

  , SL.selective lift int' int' (pure id <|> pure negate) (pure (*) <|> pure (+))
  ]

tupleTests :: TestTree
tupleTests =
  let
    lift :: Gen b -> Gen ([Integer], b)
    lift = ((,) <$> list' int' <*>)
  in
  testGroup "Tuple" $
  [ monoidal   @(×) @(×) "Applicative" lift int'
  , opmonoidal @(+) @(+) "Decisive"    lift int'
  ]

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ timesTests
  , sumTests
  , theseTests

  , maybeTests
  , listTests
  , tupleTests
  ]
