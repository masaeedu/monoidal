module Control.Category.Sub.Op where

import Prelude hiding (id, (.))
import Control.Category (Category(..))
import Control.Category.Sub.Category (SubCat(..))

newtype Op p a b = Op { runOp :: p b a }

instance Category p => Category (Op p)
  where
  id = Op $ id
  Op f . Op g = Op $ g . f

instance SubCat p => SubCat (Op p)
