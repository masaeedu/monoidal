module Data.Functor.Monoidal.Hedgehog where

import Control.Category.Tensor

import Data.Functor.Monoidal.Class
import Data.Functor.Monoidal.Laws

import Hedgehog
import Hedgehog.Extra

type TestablySemigroupal t u f =
  ( Testable2 t
  , Testable2 u
  , Testable1 f
  , Semigroupal t u f
  )

type TestablyOpSemigroupal t u f =
  ( Testable2 t
  , Testable2 u
  , Testable1 f
  , OpSemigroupal t u f
  )

type TestablyMonoidal t u f =
  ( Testable2 t
  , Testable2 u
  , Testable (Unit t)
  , Testable (Unit u)
  , Testable1 f
  , Monoidal t u f
  )

type TestablyOpMonoidal t u f =
  ( Testable2 t
  , Testable2 u
  , Testable (Unit t)
  , Testable (Unit u)
  , Testable1 f
  , OpMonoidal t u f
  )

associativity :: forall t u f a.
  ( Testable a
  , TestablySemigroupal t u f
  , Semigroupal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
associativity lift g = property $ do
  v <- forAll $ combineF (combineF (lift g, lift g), lift g)
  associativity1 @t @u v === associativity2 @t @u v

lunitality :: forall t u f a.
  ( Testable a
  , TestablyMonoidal t u f
  , Monoidal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
lunitality lift g = property $ do
  v <- forAll $ combineF (unitF @u @(×) (), lift g)
  lunitality1 @t @u v === lunitality2 @t @u v

runitality :: forall t u f a.
  ( Testable a
  , TestablyMonoidal t u f
  , Monoidal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
runitality lift g = property $ do
  v <- forAll $ combineF (lift g, unitF @u @(×) ())
  runitality1 @t @u v === runitality2 @t @u v

ldistributivity :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablySemigroupal i1 o1 f
  , TestablySemigroupal i2 o2 f
  , LaxLeftDistributive i1 i2
  , LaxLeftDistributive o1 o2
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
ldistributivity lift g = property $ do
  v <- forAll $ combineF (lift g, combineF (lift g, lift g))
  ldistributivity1 @i1 @o1 @i2 @o2 v === ldistributivity2 @i1 @o1 @i2 @o2 v

rdistributivity :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablySemigroupal i1 o1 f
  , TestablySemigroupal i2 o2 f
  , LaxRightDistributive i1 i2
  , LaxRightDistributive o1 o2
  , Semigroupal o1 (×) Gen
  , Semigroupal o2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
rdistributivity lift g = property $ do
  v <- forAll $ combineF (combineF (lift g, lift g), lift g)
  rdistributivity1 @i1 @o1 @i2 @o2 v === rdistributivity2 @i1 @o1 @i2 @o2 v

symmetry :: forall t u f a.
  ( Testable a
  , TestablySemigroupal t u f
  , Symmetric t
  , Symmetric u
  , Semigroupal u (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
symmetry lift g = property $ do
  v <- forAll $ combineF (lift g, lift g)
  symmetry1 @t @u @f v === symmetry2 @t @u @f v

opassociativity :: forall t u f a.
  ( Testable a
  , TestablyOpSemigroupal t u f
  , Semigroupal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
opassociativity lift g = property $ do
  v <- forAll $ lift $ combineF (g, combineF (g, g))
  opassociativity1 @t @u v === opassociativity2 @t @u v

oplunitality :: forall t u f a.
  ( Testable a
  , TestablyOpMonoidal t u f
  , Monoidal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
oplunitality lift g = property $ do
  v <- forAll $ lift g
  oplunitality1 @t @u v === oplunitality2 @t @u v

oprunitality :: forall t u f a.
  ( Testable a
  , TestablyOpMonoidal t u f
  , Monoidal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
oprunitality lift g = property $ do
  v <- forAll $ lift g
  oprunitality1 @t @u v === oprunitality2 @t @u v

opldistributivity :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablyOpSemigroupal i1 o1 f
  , TestablyOpSemigroupal i2 o2 f
  , OpLaxLeftDistributive i1 i2
  , OpLaxLeftDistributive o1 o2
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
opldistributivity lift g = property $ do
  v <- forAll $ lift $ combineF (combineF (g, g), combineF (g, g))
  opldistributivity1 @i1 @o1 @i2 @o2 v === opldistributivity2 @i1 @o1 @i2 @o2 v

oprdistributivity :: forall i1 o1 i2 o2 f a.
  ( Testable a
  , TestablyOpSemigroupal i1 o1 f
  , TestablyOpSemigroupal i2 o2 f
  , OpLaxRightDistributive i1 i2
  , OpLaxRightDistributive o1 o2
  , Semigroupal i1 (×) Gen
  , Semigroupal i2 (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
oprdistributivity lift g = property $ do
  v <- forAll $ lift $ combineF (combineF (g, g), combineF (g, g))
  oprdistributivity1 @i1 @o1 @i2 @o2 v === oprdistributivity2 @i1 @o1 @i2 @o2 v

opsymmetry :: forall t u f a.
  ( Testable a
  , TestablyOpSemigroupal t u f
  , Symmetric t
  , Symmetric u
  , Semigroupal t (×) Gen
  ) =>
  (forall x. Gen x -> Gen (f x)) -> Gen a -> Property
opsymmetry lift g = property $ do
  v <- forAll $ lift $ combineF (g, g)
  opsymmetry1 @t @u @f v === opsymmetry2 @t @u @f v
