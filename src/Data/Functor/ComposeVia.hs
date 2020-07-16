module Data.Functor.ComposeVia where

import Data.Functor.Compose

newtype ComposeVia m f g a = ComposeVia { getComposeVia :: f (g a) }
  deriving (Eq, Show)

deriving via (Compose (f :: * -> *) (g :: * -> *))
  instance (Functor f, Functor g) => Functor (ComposeVia m f g)
