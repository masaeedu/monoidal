module Control.Category.Iso where

import Prelude hiding ((.), id)
import Control.Category

data Iso p a b = Iso { fwd :: p a b, bwd :: p b a }

instance Category p => Category (Iso p)
  where
  id = Iso id id
  Iso a b . Iso c d = Iso (a . c) (d . b)
