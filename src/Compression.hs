{-# LANGUAGE MultiParamTypeClasses #-}

module Compression where

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
