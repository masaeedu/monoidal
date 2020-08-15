module Data.Profunctor.ProcomposeVia where

import Prelude hiding (id, (.))

import Data.Profunctor

import Control.Category
import Control.Category.Iso
import Control.Category.Tensor

data ProcomposeVia m :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> *
  where
  ProcomposeVia :: { after :: p v o, before :: q i v } -> ProcomposeVia m p q i o

instance (Profunctor p, Profunctor q) => Profunctor (ProcomposeVia m p q)
  where
  dimap f g (ProcomposeVia a b) = ProcomposeVia (rmap g a) (lmap f b)

newtype p ~~> q = Nat2 { runDinat :: forall a b. p a b -> q a b }

instance Category (~~>)
  where
  id = Nat2 id
  Nat2 f . Nat2 g = Nat2 $ f . g

instance Structure (ProcomposeVia m :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> *)
  where
  type Arrow (ProcomposeVia m) = (~~>)
  type Ask (ProcomposeVia m) = Profunctor
  bimap (Nat2 f) (Nat2 g) = Nat2 $ \(ProcomposeVia a b) -> ProcomposeVia (f a) (g b)

instance Associative (ProcomposeVia m)
  where
  assoc = Iso
    (Nat2 $ \(ProcomposeVia (ProcomposeVia a b) c) -> ProcomposeVia a (ProcomposeVia b c))
    (Nat2 $ \(ProcomposeVia a (ProcomposeVia b c)) -> ProcomposeVia (ProcomposeVia a b) c)

instance Unital (ProcomposeVia m)
  where
  type Unit (ProcomposeVia m) = (->)
  lunit' = Iso
    (Nat2 $ \(ProcomposeVia a b) -> rmap a b)
    (Nat2 $ ProcomposeVia id)
  runit' = Iso
    (Nat2 $ \(ProcomposeVia a b) -> lmap b a)
    (Nat2 $ flip ProcomposeVia id)
