module Control.Category.ProductCategory where

-- Haskell's support for type level currying and uncurrying sucks,
-- so using type level tuples and the regular Category typeclass doesn't
-- work that well. Oh well...
class ProductCategory o
  where
  bicompose :: o b b' c c' -> o a a' b b' -> o a a' c c'
  biid :: o a a' a a'

(<<<) :: ProductCategory o => o b b' c c' -> o a a' b b' -> o a a' c c'
(<<<) = bicompose

(>>>) :: ProductCategory o => o a a' b b' -> o b b' c c' -> o a a' c c'
(>>>) = flip bicompose
