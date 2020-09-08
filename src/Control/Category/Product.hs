module Control.Category.Product where

-- Haskell's support for type level currying and uncurrying sucks,
-- so using type level tuples and the regular Category typeclass doesn't
-- work that well. Oh well...
class ProductCategory p
  where
  bicompose :: p b b' c c' -> p a a' b b' -> p a a' c c'
  biid :: p a a' a a'

(<<<) :: ProductCategory p => p b b' c c' -> p a a' b b' -> p a a' c c'
(<<<) = bicompose

(>>>) :: ProductCategory p => p a a' b b' -> p b b' c c' -> p a a' c c'
(>>>) = flip bicompose
