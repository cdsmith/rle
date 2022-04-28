{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Interval (Interval) where

import Compression (Compression (..), Split (..))

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
  logicalLength (Interval i j) = j - i + 1
  logicalLength (Unique _) = 1

  solo x
    | x == toEnum i = Interval i i
    | otherwise = Unique x
    where
      i = fromEnum x

  popHead (Interval i j) =
    (toEnum i, if i == j then [] else [Interval (i + 1) j])
  popHead (Unique x) = (x, [])

  popTail (Interval i j) =
    (if i == j then [] else [Interval i (j - 1)], toEnum j)
  popTail (Unique x) = ([], x)

  tryConcat (Interval i j) (Interval k l) =
    if j + 1 == k then Just (Interval i l) else Nothing
  tryConcat _ _ = Nothing

  trySplit int@(Interval j k) i
    | i <= 0 = AllRight int
    | i > k - j = AllLeft int
    | otherwise = let m = j + i in Split [Interval i (m - 1)] [Interval m k]
  trySplit int@(Unique _) i = if i <= 0 then AllRight int else AllLeft int
