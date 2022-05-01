{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Interval (Interval(..)) where

import Compression (Compression (..))
import Text.Show.Deriving (deriveShow1)

-- | A compression scheme that encodes consecutive elements into inclusive
-- ranges. @'toList' ('Interval' lo hi) = 'map' 'toEnum' [lo .. hi]@.
-- @'Unique' x@ encodes an element @x@ that doesn't survive the round trip
-- @'toEnum' . 'fromEnum'@.
data Interval a where
  Interval :: Enum a => Int -> Int -> Interval a
  Unique :: a -> Interval a

deriving instance Eq a => Eq (Interval a)

deriving instance Ord a => Ord (Interval a)

deriving instance Show a => Show (Interval a)

deriveShow1 ''Interval

instance Foldable Interval where
  foldMap f (Interval lo hi) = foldMap (f . toEnum) [lo .. hi]
  foldMap f (Unique x) = f x

  length (Interval lo hi) = hi - lo + 1
  length (Unique _) = 1

instance (Eq a, Enum a) => Compression Interval a where
  solo x
    | x == toEnum i = Interval i i
    | otherwise = Unique x
    where
      i = fromEnum x

  popHead (Interval lo hi) = (toEnum lo, [Interval (lo + 1) hi | lo /= hi])
  popHead (Unique x) = (x, [])

  popTail (Interval lo hi) = ([Interval lo (hi - 1) | lo /= hi], toEnum hi)
  popTail (Unique x) = ([], x)

  tryMerge (Interval lo_1 hi_1) (Interval lo_2 hi_2)
    | hi_1 + 1 == lo_2 = Just (Interval lo_1 hi_2)
  tryMerge _ _ = Nothing

  split atom i
    | i <= 0 = ([], [atom])
    | Interval lo hi <- atom,
      i <= hi - lo =
        ([Interval lo (lo + i - 1)], [Interval (lo + i) hi])
    | otherwise = ([atom], [])
