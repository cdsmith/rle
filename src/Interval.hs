{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Interval (Interval(..)) where

import Compression (Compression (..))
import Text.Show.Deriving (deriveShow1)

data Interval a where
  Interval :: Enum a => Int -> Int -> Interval a
  Unique :: a -> Interval a

deriving instance Eq a => Eq (Interval a)

deriving instance Ord a => Ord (Interval a)

deriving instance Show a => Show (Interval a)

deriveShow1 ''Interval

instance Foldable Interval where
  foldMap f (Interval i j) = foldMap (f . toEnum) [i .. j]
  foldMap f (Unique x) = f x

instance (Eq a, Enum a) => Compression Interval a where
  count (Interval i j) = j - i + 1
  count (Unique _) = 1

  solo x
    | x == toEnum i = Interval i i
    | otherwise = Unique x
    where
      i = fromEnum x

  popHead (Interval i j) = (toEnum i, [Interval (i + 1) j | i /= j])
  popHead (Unique x) = (x, [])

  popTail (Interval i j) = ([Interval i (j - 1) | i /= j], toEnum j)
  popTail (Unique x) = ([], x)

  tryMerge (Interval i j) (Interval k l) =
    if j + 1 == k then Just (Interval i l) else Nothing
  tryMerge _ _ = Nothing

  split atom i
    | i <= 0 = ([], [atom])
    | Interval lo hi <- atom,
      i <= hi - lo =
        ([Interval lo (lo + i - 1)], [Interval (lo + i) hi])
    | otherwise = ([atom], [])
