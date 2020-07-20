module Data.Functor.ComposeVia where

import Prelude hiding (id, (.))

import Control.Category
import Control.Category.Tensor
import Control.Category.Iso

import Data.Functor.Identity
import Data.Functor.Compose

newtype ComposeVia m f g a = ComposeVia { getComposeVia :: f (g a) }
  deriving (Eq, Show)

deriving via (Compose (f :: * -> *) (g :: * -> *))
  instance (Functor f, Functor g) => Functor (ComposeVia m f g)

newtype f ~> g = Nat { runNat :: forall x. f x -> g x }

instance Category (~>)
  where
  id = Nat $ id
  Nat f . Nat g = Nat $ f . g

instance Structure (ComposeVia m)
  where
  type Arrow (ComposeVia m) = (~>)
  type Ask (ComposeVia m) = Functor

  bimap (Nat f) (Nat g) = Nat $ \(ComposeVia v) -> ComposeVia $ f $ fmap g $ v

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
