{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Uncompressed (Uncompressed) where

import Compression (Compression (..))

newtype Uncompressed a = Uncompressed a deriving (Eq, Ord, Show)

instance Foldable Uncompressed where
  foldMap f (Uncompressed x) = f x

instance Compression Uncompressed a where
  logicalLength _ = 1

  solo = Uncompressed

  popHead (Uncompressed x) = (x, [])
  popTail (Uncompressed x) = ([], x)

  tryMerge _ _ = Nothing

  split x i
    | i <= 0 = ([], [x])
    | otherwise = ([x], [])
