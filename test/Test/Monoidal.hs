module Test.Monoidal where

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Laws

type Testable a = (Eq a, Show a)

associativity :: forall t u f a.
  ( Semigroupal t u f
  , Show (f a `u` f a `u` f a)
  , Testable (f (a `t` (a `t` a)))
  , Semigroupal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
associativity lift g = property $ do
  v <- forAll $ combineF (combineF (lift g, lift g), lift g)
  associativity1 @t @u v === associativity2 @t @u v

lunitality :: forall t u f a.
  ( Monoidal t u f
  , Show (Unit u `u` f a)
  , Testable (f a)
  , Monoidal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
lunitality lift g = property $ do
  v <- forAll $ combineF (unitF @u @(×) (), lift g)
  lunitality1 @t @u v === lunitality2 @t @u v

runitality :: forall t u f a.
  ( Monoidal t u f
  , Show (f a `u` Unit u)
  , Testable (f a)
  , Monoidal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
runitality lift g = property $ do
  v <- forAll $ combineF (lift g, unitF @u @(×) ())
  runitality1 @t @u v === runitality2 @t @u v

ldistributivity :: forall i1 o1 i2 o2 f a.
  ( Semigroupal i1 o1 f
  , Semigroupal i2 o2 f
  , LaxLeftDistributive i1 i2
  , LaxLeftDistributive o1 o2
  , Show (f a `o1` (f a `o2` f a))
  , Testable (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
ldistributivity lift g = property $ do
  v <- forAll $ combineF (lift g, combineF (lift g, lift g))
  ldistributivity1 @i1 @o1 @i2 @o2 v === ldistributivity2 @i1 @o1 @i2 @o2 v

rdistributivity :: forall i1 o1 i2 o2 f a.
  ( Semigroupal i1 o1 f
  , Semigroupal i2 o2 f
  , LaxRightDistributive i1 i2
  , LaxRightDistributive o1 o2
  , Show ((f a `o2` f a) `o1` f a)
  , Testable (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
rdistributivity lift g = property $ do
  v <- forAll $ combineF (combineF (lift g, lift g), lift g)
  rdistributivity1 @i1 @o1 @i2 @o2 v === rdistributivity2 @i1 @o1 @i2 @o2 v

symmetry :: forall t u f a.
  ( Semigroupal t u f
  , Symmetric t
  , Symmetric u
  , Show (f a `u` f a)
  , Testable (f (a `t` a))
  , Semigroupal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
symmetry lift g = property $ do
  v <- forAll $ combineF (lift g, lift g)
  symmetry1 @t @u @f v === symmetry2 @t @u @f v

opassociativity :: forall t u f a.
  ( OpSemigroupal t u f
  , Testable (f a `u` f a `u` f a)
  , Show (f (a `t` (a `t` a)))
  , Semigroupal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
opassociativity lift g = property $ do
  v <- forAll $ lift $ combineF (g, combineF (g, g))
  opassociativity1 @t @u v === opassociativity2 @t @u v

oplunitality :: forall t u f a.
  ( OpMonoidal t u f
  , Testable (Unit u `u` f a)
  , Show (f a)
  , Monoidal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
oplunitality lift g = property $ do
  v <- forAll $ lift g
  oplunitality1 @t @u v === oplunitality2 @t @u v

oprunitality :: forall t u f a.
  ( OpMonoidal t u f
  , Testable (f a `u` Unit u)
  , Show (f a)
  , Monoidal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
oprunitality lift g = property $ do
  v <- forAll $ lift g
  oprunitality1 @t @u v === oprunitality2 @t @u v

opldistributivity :: forall i1 o1 i2 o2 f a.
  ( OpSemigroupal i1 o1 f
  , OpSemigroupal i2 o2 f
  , OpLaxLeftDistributive i1 i2
  , OpLaxLeftDistributive o1 o2
  , Testable (f a `o1` (f a `o2` f a))
  , Show (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
opldistributivity lift g = property $ do
  v <- forAll $ lift $ combineF (combineF (g, g), combineF (g, g))
  opldistributivity1 @i1 @o1 @i2 @o2 v === opldistributivity2 @i1 @o1 @i2 @o2 v

oprdistributivity :: forall i1 o1 i2 o2 f a.
  ( OpSemigroupal i1 o1 f
  , OpSemigroupal i2 o2 f
  , OpLaxRightDistributive i1 i2
  , OpLaxRightDistributive o1 o2
  , Testable ((f a `o2` f a) `o1` f a)
  , Show (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
oprdistributivity lift g = property $ do
  v <- forAll $ lift $ combineF (combineF (g, g), combineF (g, g))
  oprdistributivity1 @i1 @o1 @i2 @o2 v === oprdistributivity2 @i1 @o1 @i2 @o2 v

opsymmetry :: forall t u f a.
  ( OpSemigroupal t u f
  , Symmetric t
  , Symmetric u
  , Testable (f a `u` f a)
  , Show (f (a `t` a))
  , Semigroupal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
opsymmetry lift g = property $ do
  v <- forAll $ lift $ combineF (g, g)
  opsymmetry1 @t @u @f v === opsymmetry2 @t @u @f v

semigroupal :: forall t u f a.
  ( Semigroupal t u f
  , Show (f a `u` f a `u` f a)
  , Testable (f (a `t` (a `t` a)))
  , Semigroupal u (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
semigroupal name lift g =
  testGroup name $
  [ testProperty "Associativity" $ associativity @t @u lift g
  ]

monoidal :: forall t u f a.
  ( Monoidal t u f
  , Show (f a `u` f a `u` f a)
  , Testable (f (a `t` (a `t` a)))
  , Show (Unit u `u` f a)
  , Show (f a `u` Unit u)
  , Testable (f a)
  , Monoidal u (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
monoidal name lift g =
  testGroup name $
  [ testProperty "Associativity" $ associativity @t @u lift g
  , testProperty "Left unitality" $ lunitality @t @u lift g
  , testProperty "Right unitality" $ runitality @t @u lift g
  ]

ldistributive :: forall i1 o1 i2 o2 f a.
  ( Semigroupal i1 o1 f
  , Semigroupal i2 o2 f
  , LaxLeftDistributive i1 i2
  , LaxLeftDistributive o1 o2
  , Show (f a `o1` (f a `o2` f a))
  , Testable (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
ldistributive name lift g =
  testGroup name $
  [ testProperty "Left distributivity" $ ldistributivity @i1 @o1 @i2 @o2 lift g
  ]

rdistributive :: forall i1 o1 i2 o2 f a.
  ( Semigroupal i1 o1 f
  , Semigroupal i2 o2 f
  , LaxRightDistributive i1 i2
  , LaxRightDistributive o1 o2
  , Show ((f a `o2` f a) `o1` f a)
  , Testable (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
rdistributive name lift g =
  testGroup name $
  [ testProperty "Right distributivity" $ rdistributivity @i1 @o1 @i2 @o2 lift g
  ]

distributive :: forall i1 o1 i2 o2 f a.
  ( Semigroupal i1 o1 f
  , Semigroupal i2 o2 f
  , LaxLeftDistributive i1 i2
  , LaxLeftDistributive o1 o2
  , LaxRightDistributive i1 i2
  , LaxRightDistributive o1 o2
  , Show (f a `o1` (f a `o2` f a))
  , Show ((f a `o2` f a) `o1` f a)
  , Testable (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
distributive name lift g =
  testGroup name $
  [ testProperty "Left distributivity" $ ldistributivity @i1 @o1 @i2 @o2 lift g
  , testProperty "Right distributivity" $ rdistributivity @i1 @o1 @i2 @o2 lift g
  ]

symmetric :: forall t u f a.
  ( Semigroupal t u f
  , Symmetric t
  , Symmetric u
  , Show (f a `u` f a)
  , Testable (f (a `t` a))
  , Semigroupal u (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
symmetric name lift g =
  testGroup name $
  [ testProperty "Symmetry" $ symmetry @t @u lift g
  ]

opsemigroupal :: forall t u f a.
  ( OpSemigroupal t u f
  , Testable (f a `u` f a `u` f a)
  , Show (f (a `t` (a `t` a)))
  , Semigroupal t (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opsemigroupal name lift g =
  testGroup name $
  [ testProperty "Associativity (opposite)" $ opassociativity @t @u lift g
  ]

opmonoidal :: forall t u f a.
  ( OpMonoidal t u f
  , Testable (f a `u` f a `u` f a)
  , Show (f (a `t` (a `t` a)))
  , Testable (Unit u `u` f a)
  , Testable (f a `u` Unit u)
  , Show (f a)
  , Monoidal t (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opmonoidal name lift g =
  testGroup name $
  [ testProperty "Associativity (opposite)" $ opassociativity @t @u lift g
  , testProperty "Left unitality (opposite)" $ oplunitality @t @u lift g
  , testProperty "Right unitality (opposite)" $ oprunitality @t @u lift g
  ]

opldistributive :: forall i1 o1 i2 o2 f a.
  ( OpSemigroupal i1 o1 f
  , OpSemigroupal i2 o2 f
  , OpLaxLeftDistributive i1 i2
  , OpLaxLeftDistributive o1 o2
  , Testable (f a `o1` (f a `o2` f a))
  , Show (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opldistributive name lift g =
  testGroup name $
  [ testProperty "Left distributivity (opposite)" $ opldistributivity @i1 @o1 @i2 @o2 lift g
  ]

oprdistributive :: forall i1 o1 i2 o2 f a.
  ( OpSemigroupal i1 o1 f
  , OpSemigroupal i2 o2 f
  , OpLaxRightDistributive i1 i2
  , OpLaxRightDistributive o1 o2
  , Testable ((f a `o2` f a) `o1` f a)
  , Show (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
oprdistributive name lift g =
  testGroup name $
  [ testProperty "Right distributivity (opposite)" $ oprdistributivity @i1 @o1 @i2 @o2 lift g
  ]

opdistributive :: forall i1 o1 i2 o2 f a.
  ( OpSemigroupal i1 o1 f
  , OpSemigroupal i2 o2 f
  , OpLaxLeftDistributive i1 i2
  , OpLaxLeftDistributive o1 o2
  , OpLaxRightDistributive i1 i2
  , OpLaxRightDistributive o1 o2
  , Testable (f a `o1` (f a `o2` f a))
  , Testable ((f a `o2` f a) `o1` f a)
  , Show (f ((a `i1` a) `i2` (a `i1` a)))
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opdistributive name lift g =
  testGroup name $
  [ testProperty "Left distributivity (opposite)" $ oprdistributivity @i1 @o1 @i2 @o2 lift g
  , testProperty "Right distributivity (opposite)" $ oprdistributivity @i1 @o1 @i2 @o2 lift g
  ]

opsymmetric :: forall t u f a.
  ( OpSemigroupal t u f
  , Symmetric t
  , Symmetric u
  , Testable (f a `u` f a)
  , Show (f (a `t` a))
  , Semigroupal t (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opsymmetric name lift g =
  testGroup name $
  [ testProperty "Symmetry (opposite)" $ opsymmetry @t @u lift g
  ]
