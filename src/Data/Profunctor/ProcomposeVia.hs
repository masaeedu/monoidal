module Data.Profunctor.ProcomposeVia where

import Prelude hiding (id, (.))

import Data.Coerce

import Data.Profunctor

import Control.Category
import Control.Category.Iso
import Control.Category.Tensor
import Control.Category.Sub
import Control.Category.Product

data ProcomposeVia m :: (* -> * -> *) -> (* -> * -> *) -> * -> * -> *
  where
  ProcomposeVia :: { after :: p v o, before :: q i v } -> ProcomposeVia m p q i o

instance (Profunctor p, Profunctor q) => Profunctor (ProcomposeVia m p q)
  where
  dimap f g (ProcomposeVia a b) = ProcomposeVia (rmap g a) (lmap f b)

newtype (p :: * -> * -> *) ~~> (q :: * -> * -> *) = Nat2 { runDinat :: forall a b. p a b -> q a b }

instance Category (~~>)
  where
  id = Nat2 id
  Nat2 f . Nat2 g = Nat2 $ f . g

instance SubCat (~~>)
  where
  type Ob (~~>) = Profunctor

newtype Uncurry2 :: (a -> b -> c -> d -> *) -> (a, b) -> c -> d -> *
  where
  Uncurry2 :: { runUncurry2 :: pc (Fst pq) (Snd pq) a b } -> Uncurry2 pc pq a b

deriving instance (Profunctor (Fst x), Profunctor (Snd x)) => Profunctor (Uncurry2 (ProcomposeVia m) x)

instance GFunctor (BiArrow (~~>) (~~>)) (~~>) (Uncurry2 (ProcomposeVia m))
  where
  gfmap (BiArrow (Nat2 f) (Nat2 g)) = Nat2 $ Uncurry2 . (\(ProcomposeVia a b) -> ProcomposeVia (f a) (g b)) . runUncurry2

instance GBifunctor (~~>) (ProcomposeVia m)
  where
  type Uncurry (ProcomposeVia m) = Uncurry2 (ProcomposeVia m)
  uncurryB = Nat2 coerce
  curryB = Nat2 coerce

instance Structure (ProcomposeVia m)
  where
  type Arrow (ProcomposeVia m) = (~~>)

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
