module Control.Category.Iso.Laws where

import Prelude hiding (id)

import Control.Category
import Control.Category.Iso

fwdid1, fwdid2 ::
  Category p =>
  Iso p a b ->
  a `p` a
fwdid1 (Iso x y) = y <<< x
fwdid2 _         = id

bwdid1, bwdid2 ::
  Category p =>
  Iso p a b ->
  b `p` b
bwdid1 (Iso x y) = x <<< y
bwdid2 _         = id
