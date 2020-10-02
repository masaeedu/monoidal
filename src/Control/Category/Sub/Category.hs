module Control.Category.Sub.Category where

import Data.Subtypes (Trivial)
import GHC.Exts (Constraint)
import Control.Category (Category(..))

class Category p => SubCat (p :: k -> k -> *)
  where
  type Ob p :: k -> Constraint
  type Ob p = Trivial

instance SubCat (->)
