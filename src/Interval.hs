{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interval (Interval) where

import Compression (Compression (..))
import Data.These (These (..))

data Interval a = Interval a a deriving (Eq, Ord, Show)

instance (Eq a, Enum a) => Compression Interval a where
  logicalLength (Interval a b) = fromEnum b - fromEnum a + 1

  solo x = Interval x x

  popHead (Interval a b) =
    (a, if a == b then Nothing else Just (Interval (succ a) b))

  popTail (Interval a b) =
    (if a == b then Nothing else Just (Interval a (pred b)), b)

  tryConcat (Interval a b) (Interval c d) =
    if succ b == c then Just (Interval a d) else Nothing

  trySplit int@(Interval a b) i
    | i <= 0 = That int
    | i >= logicalLength int = This int
    | otherwise = let mid = toEnum (fromEnum a + i) in These (Interval a (pred mid)) (Interval mid b)
