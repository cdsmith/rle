{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Interval (Interval) where

import Compression (Compression (..))
import Data.These (These (..))

data Interval a where
  Interval :: Enum a => Int -> Int -> Interval a
  Unique :: a -> Interval a

deriving instance Eq a => Eq (Interval a)

deriving instance Ord a => Ord (Interval a)

deriving instance Show a => Show (Interval a)

instance Foldable Interval where
  foldMap f (Interval i j) = foldMap (f . toEnum) [i .. j]
  foldMap f (Unique x) = f x

instance (Eq a, Enum a) => Compression Interval a where
  logicalLength (Interval i j) = i - j + 1
  logicalLength (Unique _) = 1

  solo x
    | x == toEnum i = Interval i i
    | otherwise = Unique x
    where
      i = fromEnum x

  popHead (Interval i j) =
    (toEnum i, if i == j then Nothing else Just (Interval (i + 1) j))
  popHead (Unique x) = (x, Nothing)

  popTail (Interval i j) =
    (if i == j then Nothing else Just (Interval i (j - 1)), toEnum j)
  popTail (Unique x) = (Nothing, x)

  tryConcat (Interval i j) (Interval k l) =
    if j + 1 == k then Just (Interval i l) else Nothing
  tryConcat _ _ = Nothing

  trySplit int@(Interval j k) i
    | i <= 0 = That int
    | i > k - j = This int
    | otherwise = let m = j + i in These (Interval i (m - 1)) (Interval m k)
  trySplit int@(Unique _) i = if i <= 0 then That int else This int
