module Data.Biprofunctor.Monoidal.Class where

import Control.Category.Tensor.Hask
import Data.Biprofunctor

class
  ( Associative t1
  , Associative t2
  , Associative t3
  , Associative t4
  , Associative o
  , Biprofunctor bp
  ) =>
  Semigroupal t1 t2 t3 t4 o bp
  where
  combineBP :: bp a b s t `o` bp a' b' s' t' -> bp (t1 a a') (t2 b b') (t3 s s') (t4 t t')

(-?-) :: Semigroupal t1 t2 t3 t4 (×) bp => bp a b s t -> bp a' b' s' t' -> bp (t1 a a') (t2 b b') (t3 s s') (t4 t t')
(-?-) = curry combineBP

class
  ( Tensor t1
  , Tensor t2
  , Tensor t3
  , Tensor t4
  , Tensor o
  , Semigroupal t1 t2 t3 t4 o bp
  ) =>
  Monoidal t1 t2 t3 t4 o bp
  where
  unitBP :: Unit o -> bp (Unit t1) (Unit t2) (Unit t3) (Unit t4)
