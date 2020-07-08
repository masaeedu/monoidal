module Data.Biprofunctor.Re where

import Data.Void

import Control.Category.ProductCategory
import Control.Category.Tensor

import Data.Bifunctor

import Data.Biprofunctor
import Data.Biprofunctor.Monoidal.Class

import Data.Profunctor
import qualified Data.Profunctor.Monoidal.Class as P

newtype Re bp t s b a = Re { runRe :: bp a b s t }

instance Biprofunctor bp => Biprofunctor (Re bp)
  where
  bidimap f g h i (Re o) = Re $ bidimap i h g f o

instance
  Semigroupal t1 t2 t3 t4 o bp =>
  Semigroupal t4 t3 t2 t1 o (Re bp)
  where
  combineBP = Re . combineBP . bimap runRe runRe

instance
  Monoidal t1 t2 t3 t4 o bp =>
  Monoidal t4 t3 t2 t1 o (Re bp)
  where
  unitBP = Re . unitBP @t1 @t2 @t3 @t4 @o

instance ProductCategory bp => ProductCategory (Re bp)
  where
  biid = Re $ biid
  bicompose (Re f) (Re g) = Re $ bicompose g f

instance Biprofunctor bp => Profunctor (Re bp t s)
  where
  dimap f g (Re p) = Re $ bidimap id id g f p

instance Semigroupal t2 t1 (×) (+) o bp => P.Semigroupal t1 t2 o (Re bp t s)
  where
  combineP = Re . bidimap (\x -> (x, x)) (either id id) id id . combineBP @t2 @t1 @(×) @(+) @o . bimap runRe runRe

instance Monoidal t2 t1 (×) (+) o bp => P.Monoidal t1 t2 o (Re bp t s)
  where
  unitP = Re . bidimap (const ()) absurd id id . unitBP @t2 @t1 @(×) @(+) @o
