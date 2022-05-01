{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compression (Compression (..)) where

import Data.Coerce (coerce)
import Data.Foldable (foldMap')
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..))

-- | Type class representing a compression scheme.  When there is an instance
-- for @Compression f a@, then @f a@ is a compressed representation for some
-- (but not all) non-empty sequences with elements of type @a@.  We refer to a
-- value of type @f a@ as an "atom".
--
-- LAWS:
--
-- Part 1: Compatibility with Foldable.
--
-- These first laws ensure that Compression operations behave as expected on
-- the elements represented by an atom.
--
-- * @count = length@
-- * @toList (solo x) = [x]@
-- * If @popHead xs = (y, yss)@, then @toList xs = y : concatMap toList yss@
-- * If @popTail xs = (yss, y)@, then @toList xs = concatMap toList yss ++ [y]@
-- * If @tryMerge xs ys = Just zs@, then @toList xs ++ toList ys = toList zs@
-- * If @split xs i = (ys, zs)@, then
--   @splitAt i (toList xs) = (toList ys, toList zs)@
--
-- Part 2: Confluence (In progress)
--
-- These second laws are more optional, and we refer to an instance that
-- satisfies them as a "confluent" instance.  If they hold, then there is a
-- unique compressed representation that will be chosen by CompressedSeq for
-- any sequence of elements, regardless of which operations are performed to
-- reach that sequence.
--
-- * If @tryMerge a b = Just ab@ and @tryMerge b c = Just bc@, then
--   @tryMerge a bc = tryMerge ab c /= Nothing@
--
-- Note that the three core instances included in this library, RLE, Interval,
-- and Uncompressed, are confluent.  However, it looks challenging to maintain
-- confluence with other instances.  In particular, a confluent instance for
-- Compose is tricky.
--
-- TODO: Prove these properties of CompressedSeq:
--
-- 1. (a <> b) <> c = a <> (b <> c)
-- 2. uncurry (<>) (splitAt i xs) = xs
--
-- We should get a weak version of these properties, guaranteeing that the left
-- and right sides contain the same elements, by assuming only the laws in Part
-- 1 above.  We should get a strong version, in which the two sides are equal
-- in representation as well, by also including the laws in Part 2.
class Foldable f => Compression f a where
  solo :: a -> f a
  popHead :: f a -> (a, [f a])
  popTail :: f a -> ([f a], a)
  tryMerge :: f a -> f a -> Maybe (f a)
  split :: f a -> Int -> ([f a], [f a])

  -- | I've included this only because 'length' is inefficient in the 'Foldable'
  -- instance for 'Data.Functor.Compose'.
  count :: f a -> Int
  count = length

instance Compression Identity a where
  solo = Identity

  popHead (Identity x) = (x, [])
  popTail (Identity x) = ([], x)

  tryMerge _ _ = Nothing

  split x i
    | i <= 0 = ([], [x])
    | otherwise = ([x], [])

-- | An incorrect Compression instance for Compose.
--
-- Here's the fundamental problem.  Suppose:
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
-- If they were, it should yield [Run (Interval 1 3) 2, Run (Interval 1 4) 1],
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
  split (Compose x) i = splitComposeSingle i x

splitComposeSingle ::
  (Compression g a, Compression f (g a)) =>
  Int ->
  f (g a) ->
  ([Compose f g a], [Compose f g a])
splitComposeSingle i x
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
                  (b1, b2) = splitComposeMulti i' as
               in (Compose (solo a) : b1, b2)

splitComposeMulti ::
  (Compression g a, Compression f (g a)) =>
  Int ->
  [f (g a)] ->
  ([Compose f g a], [Compose f g a])
splitComposeMulti _ [] = ([], [])
splitComposeMulti i (x : xs)
  | i < count (Compose x) =
      let (x1, x2) = splitComposeSingle i x
       in (x1, x2 ++ coerce xs)
  | otherwise =
      let i' = i - count x
          (b1, b2) = splitComposeMulti i' xs
       in (Compose x : b1, b2)
