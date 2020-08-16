module Data.Biprofunctor where

-- A functor: Hask × Hask^op × Hask^op × Hask → Hask
class Biprofunctor o
  where
  bidimap :: (a -> a') -> (b' -> b) -> (s' -> s) -> (t -> t') -> o a b s t -> o a' b' s' t'

bilmap1 :: Biprofunctor o => (s' -> s) -> o a b s t -> o a b s' t
bilmap1 f = bidimap id id f id

birmap1 :: Biprofunctor o => (t -> t') -> o a b s t -> o a b s t'
birmap1 f = bidimap id id id f

bilmap2 :: Biprofunctor o => (a -> a') -> o a b s t -> o a' b s t
bilmap2 f = bidimap f id id id

birmap2 :: Biprofunctor o => (b' -> b) -> o a b s t -> o a b' s t
birmap2 f = bidimap id f id id
