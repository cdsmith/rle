{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module RLE (RLE(..)) where

import Compression (Compression (..))
import Data.Semigroup (stimes)
import Data.Eq.Deriving (deriveEq1)
import Text.Show.Deriving (deriveShow1)

-- | A compression scheme that combined repeated elements into runs.
-- @'toList' ('Run' x n) = 'replicate' n x@.
data RLE a = Run a !Int deriving (Eq, Ord, Show)

deriveEq1 ''RLE
deriveShow1 ''RLE

instance Foldable RLE where
  foldMap f (Run x n) = stimes n (f x)
  length (Run _ n) = n
  null _ = False

instance Eq a => Compression RLE a where
  solo x = Run x 1

  popHead (Run x 1) = (x, [])
  popHead (Run x n) = (x, [Run x (n - 1)])

  popTail (Run x 1) = ([], x)
  popTail (Run x n) = ([Run x (n - 1)], x)

  tryMerge (Run x n) (Run y m) | x == y = Just (Run x (n + m))
  tryMerge _ _ = Nothing

  split (Run x n) i
    | i <= 0 = ([], [Run x n])
    | i >= n = ([Run x n], [])
    | otherwise = ([Run x i], [Run x (n - i)])
