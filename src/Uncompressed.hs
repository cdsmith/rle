{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Uncompressed (Uncompressed) where

import Compression (Compression (..))
import Data.These (These (..))

newtype Uncompressed a = Uncompressed a deriving (Eq, Ord, Show)

instance Foldable Uncompressed where
  foldMap f (Uncompressed x) = f x

instance Compression Uncompressed a where
  logicalLength _ = 1

  solo = Uncompressed

  popHead (Uncompressed x) = (x, [])
  popTail (Uncompressed x) = ([], x)

  tryConcat _ _ = Nothing

  trySplit x i
    | i <= 0 = That x
    | otherwise = This x
