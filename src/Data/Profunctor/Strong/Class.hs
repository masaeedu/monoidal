module Data.Profunctor.Strong.Class where

import Data.Profunctor (Profunctor(..))
import qualified Data.Profunctor as P

import Data.Profunctor.ProcomposeVia

import Control.Category.Tensor.Hask

-- The basic idea of strength is taken from http://www.cse.chalmers.se/~rjmh/Papers/arrows.pdf
-- We think about the bicategory Prof, in which 0-cells are categories, a 1-cell `C -|-> D` is
-- a profunctor `D^op × C -> Set`, and 2-cells are natural transformations between profunctors.
--
-- In this context, given a profunctor between "monoidal 0-cells" (i.e. monoidal categories), we
-- can produce the 2-cell analogous to a strength in Cat by appropriately lifting the monoidal
-- structures into profunctors using the star profunctor (of two covariant type arguments).
--
-- The reason we have only two monoidal structures instead of three is because the bicategory Prof
-- is defined in terms of profunctors `D^op × C -> Set` where the output category is fixed to Set.
-- This can probably be generalized to any category where we can take a coend, but that's beyond
-- the scope of this abstraction.
class
  ( Profunctor p
  , Tensor l
  , Tensor r
  ) =>
  LStrong l r p
  where
  lstrength :: p a b -> p (l a x) (r b x)

class
  ( Profunctor p
  , Tensor l
  , Tensor r
  ) =>
  RStrong l r p
  where
  rstrength :: p a b -> p (l x a) (r x b)

type Strong l r p = (LStrong l r p, RStrong l r p)

-- TODO: Figure out the categorical meaning for these ones, haven't been able to reconcile
-- this with what happens in Cat with strength in opposite 2-category
class
  ( Profunctor p
  , Tensor l
  , Tensor r
  ) =>
  OpRStrong l r p
  where
  oprstrength :: p (l x a) (r x b) -> p a b

class
  ( Profunctor p
  , Tensor l
  , Tensor r
  ) =>
  OpLStrong l r p
  where
  oplstrength :: p (l a x) (r b x) -> p a b

type OpStrong l r p = (OpLStrong l r p, OpRStrong l r p)

instance {-# OVERLAPPABLE #-} P.Strong p => LStrong (×) (×) p
  where
  lstrength = P.first'

instance {-# OVERLAPPABLE #-} P.Strong p => RStrong (×) (×) p
  where
  rstrength = P.second'

instance {-# OVERLAPPABLE #-} P.Choice p => LStrong (+) (+) p
  where
  lstrength = P.left'

instance {-# OVERLAPPABLE #-} P.Choice p => RStrong (+) (+) p
  where
  rstrength = P.right'

-- The hom-profunctor functions as the identity 1-cell in the
-- bicategory Prof
instance
  Tensor i =>
  LStrong i i (->)
  where
  lstrength = first

instance
  Tensor i =>
  RStrong i i (->)
  where
  rstrength = second

-- Coend-based profunctor composition functions as 1-cell multiplication
-- in the bicategory Prof
instance
  ( LStrong x o p
  , LStrong i x q
  ) =>
  LStrong i o (ProcomposeVia x p q)
  where
  lstrength (ProcomposeVia p q) = ProcomposeVia (lstrength @x p) (lstrength q)

instance
  ( RStrong x o p
  , RStrong i x q
  ) =>
  RStrong i o (ProcomposeVia x p q)
  where
  rstrength (ProcomposeVia p q) = ProcomposeVia (rstrength @x p) (rstrength q)
