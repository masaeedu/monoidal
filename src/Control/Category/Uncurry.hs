module Control.Category.Uncurry where

import Control.Category.Product

newtype Uncurry0 :: (a -> b -> *) -> (a, b) -> *
  where
  Uncurry0 :: { runUncurry0 :: t (Fst ab) (Snd ab) } -> Uncurry0 t ab

newtype Uncurry1 :: (a -> b -> c -> *) -> (a, b) -> c -> *
  where
  Uncurry1 :: { runUncurry1 :: t (Fst ab) (Snd ab) c } -> Uncurry1 t ab c

newtype Uncurry2 :: (a -> b -> c -> d -> *) -> (a, b) -> c -> d -> *
  where
  Uncurry2 :: { runUncurry2 :: t (Fst ab) (Snd ab) c d } -> Uncurry2 t ab c d
