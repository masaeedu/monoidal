module Data.Profunctor.ProcomposeVia where

import Data.Profunctor

import Control.Category.Iso

data ProcomposeVia m p q i o
  = forall v. ProcomposeVia { after :: p v o, before :: q i v }

instance (Profunctor p, Profunctor q) => Profunctor (ProcomposeVia m p q)
  where
  dimap f g (ProcomposeVia a b) = ProcomposeVia (rmap g a) (lmap f b)

newtype p ~~> q = Dinat { runDinat :: forall a b. p a b -> q a b }

proassoc :: Profunctor p => Iso (~~>) (ProcomposeVia m x (ProcomposeVia m y z)) (ProcomposeVia m (ProcomposeVia m x y) z)
proassoc = Iso
  (Dinat $ \(ProcomposeVia a (ProcomposeVia b c)) -> ProcomposeVia (ProcomposeVia a b) c)
  (Dinat $ \(ProcomposeVia (ProcomposeVia a b) c) -> ProcomposeVia a (ProcomposeVia b c))

prolunit :: Profunctor p => Iso (~~>) (ProcomposeVia m (->) p) p
prolunit = Iso
  (Dinat $ \(ProcomposeVia a b) -> rmap a b)
  (Dinat $ ProcomposeVia id)

prorunit :: Profunctor p => Iso (~~>) (ProcomposeVia m p (->)) p
prorunit = Iso
  (Dinat $ \(ProcomposeVia a b) -> lmap b a)
  (Dinat $ flip ProcomposeVia id)
