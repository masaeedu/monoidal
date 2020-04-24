module Data.Profunctor.Monoidal.Switch where

import Control.Category.Tensor
import Data.Profunctor.Monoidal.Class
import Data.Profunctor
import Data.Void

switch :: Switch p => p a b -> p c d -> p (a × c) (b + d)
switch = curry combineP

universal :: Universal p => p a b
universal = dimap (const ()) absurd $ unitP @(×) @(+) @(×) ()

union :: Switch p => p x a -> p x b -> p x (a + b)
union = (lmap (\x -> (x, x)) .) . switch

divide :: Switch p => p a x -> p b x -> p (a × b) x
divide = (rmap (either id id) .) . switch
