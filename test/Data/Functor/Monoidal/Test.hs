module Data.Functor.Monoidal.Test where

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import Hedgehog.Extra

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Hedgehog

semigroupal :: forall t u f a.
  ( Testable a
  , TestablySemigroupal t u f
  , Semigroupal u (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
semigroupal name lift g =
  testGroup name $
  [ testProperty "Associativity" $ associativity @t @u lift g
  ]

monoidal :: forall t u f a.
  ( Testable a
  , TestablyMonoidal t u f
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
  ( Testable a
  , TestablySemigroupal i1 o1 f
  , TestablySemigroupal i2 o2 f
  , LaxLDistrib i1 i2
  , LaxLDistrib o1 o2
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
ldistributive name lift g =
  testGroup name $
  [ testProperty "Left distributivity" $ ldistributivity @i1 @o1 @i2 @o2 lift g
  ]

rdistributive :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablySemigroupal i1 o1 f
  , TestablySemigroupal i2 o2 f
  , LaxRDistrib i1 i2
  , LaxRDistrib o1 o2
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
rdistributive name lift g =
  testGroup name $
  [ testProperty "Right distributivity" $ rdistributivity @i1 @o1 @i2 @o2 lift g
  ]

distributive :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablySemigroupal i1 o1 f
  , TestablySemigroupal i2 o2 f
  , LaxLDistrib i1 i2
  , LaxLDistrib o1 o2
  , LaxRDistrib i1 i2
  , LaxRDistrib o1 o2
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
  ( Testable a
  , TestablySemigroupal t u f
  , Symmetric t
  , Symmetric u
  , Semigroupal u (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
symmetric name lift g =
  testGroup name $
  [ testProperty "Symmetry" $ symmetry @t @u lift g
  ]

opsemigroupal :: forall t u f a.
  ( Testable a
  , TestablyOpSemigroupal t u f
  , Semigroupal t (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opsemigroupal name lift g =
  testGroup name $
  [ testProperty "Associativity (opposite)" $ opassociativity @t @u lift g
  ]

opmonoidal :: forall t u f a.
  ( Testable a
  , TestablyOpMonoidal t u f
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
  ( Testable a
  , TestablyOpSemigroupal i1 o1 f
  , TestablyOpSemigroupal i2 o2 f
  , OpLaxLDistrib i1 i2
  , OpLaxLDistrib o1 o2
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opldistributive name lift g =
  testGroup name $
  [ testProperty "Left distributivity (opposite)" $ opldistributivity @i1 @o1 @i2 @o2 lift g
  ]

oprdistributive :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablyOpSemigroupal i1 o1 f
  , TestablyOpSemigroupal i2 o2 f
  , OpLaxRDistrib i1 i2
  , OpLaxRDistrib o1 o2
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
oprdistributive name lift g =
  testGroup name $
  [ testProperty "Right distributivity (opposite)" $ oprdistributivity @i1 @o1 @i2 @o2 lift g
  ]

opdistributive :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablyOpSemigroupal i1 o1 f
  , TestablyOpSemigroupal i2 o2 f
  , OpLaxLDistrib i1 i2
  , OpLaxLDistrib o1 o2
  , OpLaxRDistrib i1 i2
  , OpLaxRDistrib o1 o2
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
  ( Testable a
  , TestablyOpSemigroupal t u f
  , Symmetric t
  , Symmetric u
  , Semigroupal t (×) Gen
  ) =>
  String -> (forall x. Gen x -> Gen (f x)) -> Gen a -> TestTree
opsymmetric name lift g =
  testGroup name $
  [ testProperty "Symmetry (opposite)" $ opsymmetry @t @u lift g
  ]
