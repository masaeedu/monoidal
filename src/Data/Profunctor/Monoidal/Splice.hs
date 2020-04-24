module Data.Profunctor.Monoidal.Splice where

import Control.Category.Tensor
import Data.Profunctor.Monoidal.Class
import Data.Void

splice :: Splice p => p a b -> p c d -> p (a + c) (b × d)
splice = curry combineP

unique :: Unique p => p Void ()
unique = unitP @(+) @(×) @(×) ()
