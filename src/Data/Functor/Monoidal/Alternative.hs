module Data.Functor.Monoidal.Alternative where

import qualified Control.Applicative as A

import Control.Category.Tensor
import Data.Functor.Monoidal.Class
import Data.Void

union :: Alt f => f a -> f b -> f (a + b)
union = curry combineF

empty :: Alternative f => f a
empty = absurd <$> unitF @(+) @(×) ()

(<|>) :: Alt f => f a -> f a -> f a
(<|>) fa fb = either id id <$> fa `union` fb

infixl 3 <|>

instance A.Alternative f => Semigroupal (+) (×) f
  where
  combineF (fa, fb) = (Left <$> fa) A.<|> (Right <$> fb)

instance A.Alternative f => Monoidal (+) (×) f
  where
  unitF _ = A.empty
