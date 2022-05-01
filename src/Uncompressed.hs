{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Uncompressed (Uncompressed (..)) where

import Compression (Compression (..))
import Text.Show.Deriving (deriveShow1)

-- | A null compression scheme.  @'toList' ('Uncompressed' x) = [x]@.
newtype Uncompressed a = Uncompressed a deriving (Eq, Ord, Show)

deriveShow1 ''Uncompressed

instance Foldable Uncompressed where
  foldMap f (Uncompressed x) = f x
  length (Uncompressed _) = 1

instance Compression Uncompressed a where
  solo = Uncompressed

  popHead (Uncompressed x) = (x, [])
  popTail (Uncompressed x) = ([], x)

  tryMerge _ _ = Nothing

  split x i
    | i <= 0 = ([], [x])
    | otherwise = ([x], [])
