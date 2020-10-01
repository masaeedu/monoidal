module Data.Functor.ComposeVia where

import Prelude hiding (id, (.))

import Data.Coerce

import Control.Category
import Control.Category.Tensor
import Control.Category.Iso

import Data.Functor.Identity
import Data.Functor.Compose

newtype ComposeVia m f g a = ComposeVia { getComposeVia :: f (g a) }
  deriving (Eq, Show)

deriving via (Compose (f :: * -> *) (g :: * -> *))
  instance (Functor f, Functor g) => Functor (ComposeVia m f g)

newtype (f :: * -> *) ~> (g :: * -> *) = Nat { runNat :: forall x. f x -> g x }

instance Category (~>)
  where
  id = Nat $ id
  Nat f . Nat g = Nat $ f . g

instance SubCat (~>)
  where
  type Ob (~>) = Functor

newtype Uncurry1 (t :: x -> y -> k -> *) :: (x, y) -> k -> *
  where
  Uncurry1 :: { runUncurry1 :: t (Fst ab) (Snd ab) v } -> Uncurry1 t ab v

deriving instance (Functor (Fst fg), Functor (Snd fg)) => Functor (Uncurry1 (ComposeVia m) fg)

instance GFunctor (BiArrow (~>) (~>)) (~>) (Uncurry1 (ComposeVia m))
  where
  gfmap (BiArrow (Nat f) (Nat g)) = Nat $ Uncurry1 . (\(ComposeVia fga) -> ComposeVia $ f $ fmap g fga) . runUncurry1

instance Structure (ComposeVia m)
  where
  type Arrow (ComposeVia m) = (~>)
  type Uncurry (ComposeVia m) = Uncurry1 (ComposeVia m)
  uncurryTensor = Nat $ coerce
  curryTensor = Nat $ coerce

instance Associative (ComposeVia m)
  where
  assoc = Iso
    (Nat $ ComposeVia . fmap ComposeVia . getComposeVia . getComposeVia)
    (Nat $ ComposeVia . ComposeVia . fmap getComposeVia . getComposeVia)

instance Unital (ComposeVia m)
  where
  type Unit (ComposeVia m) = Identity
  lunit' = Iso
    (Nat $ runIdentity . getComposeVia)
    (Nat $ ComposeVia . Identity)

  runit' = Iso
    (Nat $ fmap runIdentity . getComposeVia)
    (Nat $ ComposeVia . fmap Identity)
