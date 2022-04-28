{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RLE (RLE) where

import Compression (Compression (..))
import Data.Semigroup (stimes)
import Data.These (These (..))

data RLE a = Run a !Int deriving (Eq, Ord, Show)

instance Foldable RLE where
  foldMap f (Run x n) = stimes n (f x)

instance Eq a => Compression RLE a where
  logicalLength (Run _ n) = n

  solo x = Run x 1

  popHead (Run x 1) = (x, [])
  popHead (Run x n) = (x, [Run x (n - 1)])

  popTail (Run x 1) = ([], x)
  popTail (Run x n) = ([Run x (n - 1)], x)

  tryConcat (Run x n) (Run y m) =
    if x == y
      then Just $ Run x (n + m)
      else Nothing

  trySplit (Run x n) i
    | i <= 0 = That (Run x n)
    | i >= n = This (Run x n)
    | otherwise = These (Run x i) (Run x (n - i))
