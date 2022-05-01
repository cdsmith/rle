{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MultiCompress () where

import Compression (Compression (..))
import Data.Coerce (coerce)
import Data.Foldable (foldMap')
import Data.Functor.Compose (Compose (..))
import Data.Monoid (Sum (..))

-- Here's the fundamental problem:
--
-- Suppose
--
-- xs = [1, 2, 3, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4]
--
-- Consider a CompressedSeq using @Compose RLE Interval@.  It's natural to
-- expect Interval to be applied first, followed by RLE on the intervals.  That
-- leads to this representation:
--
-- CompressedSeq.fromList xs ~ [Run (Interval 1 3) 2, Run (Interval 1 4) 2]
--
-- However, consider splitting xs as follows:
--
-- ys = [1, 2, 3, 1, 2, 3, 1, 2, 3]
-- zs = [4, 1, 2, 3, 4]
--
-- Then the same inside-first approach naturally gives:
--
-- CompressedSeq.fromList ys ~ [Run (Interval 1 3) 3]
-- CompressedSeq.fromList zs ~ [Run (Interval 4 4) 1, Run (Interval 1 4) 1]
--
-- Compression usually happens by merging consecutive atoms, and in this case,
-- the atoms that need merging are (Run (Interval 1 3) 3) and
-- (Run (Interval 4 4) 1).  It's not obvious that they can or should be merged.
-- If they were, it would yield [Run (Interval 1 3) 2, Run (Interval 1 4) 1],
-- which is two atoms instead of 1, and isn't obviously any better than the
-- original two atoms!

instance
  (Compression f (g a), Compression g a) =>
  Compression (Compose f g) a
  where
  count (Compose xs) =
    getSum (foldMap' (Sum . count) xs)
  solo = Compose . solo . solo
  popHead (Compose x) = (b, coerce (map solo bs ++ as))
    where
      (a, as) = popHead x
      (b, bs) = popHead a
  popTail (Compose x) = (coerce (as ++ map solo bs), b)
    where
      (as, a) = popTail x
      (bs, b) = popTail a
  tryMerge (Compose x) (Compose y) = coerce (tryMerge x y)
  split (Compose x) i = splitSingle i x

splitSingle ::
  (Compression g a, Compression f (g a)) =>
  Int ->
  f (g a) ->
  ([Compose f g a], [Compose f g a])
splitSingle i x
  | i <= 0 = ([], [Compose x])
  | otherwise =
      let (a, as) = popHead x
       in if i <= count a
            then
              let (a1, a2) = split a i
               in ( map (Compose . solo) a1,
                    map (Compose . solo) a2 ++ coerce as
                  )
            else
              let i' = i - count a
                  (b1, b2) = splitMulti i' as
               in (Compose (solo a) : b1, b2)

splitMulti ::
  (Compression g a, Compression f (g a)) =>
  Int ->
  [f (g a)] ->
  ([Compose f g a], [Compose f g a])
splitMulti _ [] = ([], [])
splitMulti i (x : xs)
  | i < count (Compose x) =
      let (x1, x2) = splitSingle i x
       in (x1, x2 ++ coerce xs)
  | otherwise =
      let i' = i - count x
          (b1, b2) = splitMulti i' xs
       in (Compose x : b1, b2)
