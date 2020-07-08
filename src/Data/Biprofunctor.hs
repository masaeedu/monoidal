module Data.Biprofunctor where

class Biprofunctor o
  where
  bidimap :: (s' -> s) -> (t -> t') -> (a -> a') -> (b' -> b) -> o a b s t -> o a' b' s' t'

bilmap1 :: Biprofunctor o => (s' -> s) -> o a b s t -> o a b s' t
bilmap1 f = bidimap f id id id

birmap1 :: Biprofunctor o => (t -> t') -> o a b s t -> o a b s t'
birmap1 f = bidimap id f id id

bilmap2 :: Biprofunctor o => (b' -> b) -> o a b s t -> o a b' s t
bilmap2 f = bidimap id id id f

birmap2 :: Biprofunctor o => (a -> a') -> o a b s t -> o a' b s t
birmap2 f = bidimap id id f id
