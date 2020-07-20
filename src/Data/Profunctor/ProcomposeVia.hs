module Data.Profunctor.ProcomposeVia where

import Prelude hiding (id, (.))

import Data.Profunctor

import Control.Category
import Control.Category.Iso
import Control.Category.Tensor

data ProcomposeVia m p q i o
  = forall v. ProcomposeVia { after :: p v o, before :: q i v }

instance (Profunctor p, Profunctor q) => Profunctor (ProcomposeVia m p q)
  where
  dimap f g (ProcomposeVia a b) = ProcomposeVia (rmap g a) (lmap f b)

newtype p ~~> q = Dinat { runDinat :: forall a b. p a b -> q a b }

instance Category (~~>)
  where
  id = Dinat id
  Dinat f . Dinat g = Dinat $ f . g

instance Structure (ProcomposeVia m :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> *)
  where
  type Arrow (ProcomposeVia m) = (~~>)
  type Ask (ProcomposeVia m) = Profunctor
  bimap (Dinat f) (Dinat g) = Dinat $ \(ProcomposeVia a b) -> ProcomposeVia (f a) (g b)

instance Associative (ProcomposeVia m :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> *)
  where
  assoc = Iso
    (Dinat $ \(ProcomposeVia (ProcomposeVia a b) c) -> ProcomposeVia a (ProcomposeVia b c))
    (Dinat $ \(ProcomposeVia a (ProcomposeVia b c)) -> ProcomposeVia (ProcomposeVia a b) c)

instance Unital (ProcomposeVia m :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> *)
  where
  type Unit (ProcomposeVia m) = (->)
  lunit' = Iso
    (Dinat $ \(ProcomposeVia a b) -> rmap a b)
    (Dinat $ ProcomposeVia id)
  runit' = Iso
    (Dinat $ \(ProcomposeVia a b) -> lmap b a)
    (Dinat $ flip ProcomposeVia id)
