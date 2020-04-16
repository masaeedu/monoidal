module Control.Category.Op where

import Prelude hiding ((.), id)
import Control.Category

newtype Op p a b = Op { runOp :: p b a }

instance Category p => Category (Op p)
  where
  id = Op id
  Op f . Op g = Op $ g . f
