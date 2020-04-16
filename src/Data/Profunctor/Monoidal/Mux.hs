module Data.Profunctor.Monoidal.Mux where

import Control.Category.Tensor
import Data.Profunctor.Monoidal.Class
import Data.Profunctor

mux :: Mux p => p a b -> p c d -> p (a × c) (b × d)
mux = curry combineP

terminal :: Terminal p => p a ()
terminal = dimap (const ()) id $ unitP ()

zip :: Mux p => p x a -> p x b -> p x (a × b)
zip = (lmap (\x -> (x, x)) .) . mux
