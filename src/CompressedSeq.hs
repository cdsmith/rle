{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module CompressedSeq
  ( CompressedSeq (Empty, (:<|), (:|>)),
    atoms,
    singleton,
    fromList,
    null,
    replicate,
    replicateM,
    length,
    lookup,
    splitAt,
    adjust,
    update,
    take,
    drop,
    insertAt,
    deleteAt,
  )
where

import Compression
import qualified Control.Monad as Monad
import Data.FingerTree (FingerTree, Measured (..))
import qualified Data.FingerTree as FingerTree
import qualified Data.List as List
import Data.These (These (..))
import GHC.Generics (Generic)
import Prelude
  ( Applicative (..),
    Bool (..),
    Eq (..),
    Foldable (foldMap),
    Int,
    Maybe (..),
    Monoid (..),
    Num (..),
    Ord (..),
    Semigroup (..),
    Show (..),
    const,
    fst,
    snd,
    (.),
    (<$>),
  )

newtype CompressedSeq f a = RLESequence
  { atoms :: FingerTree Length (f a)
  }
  deriving stock (Eq, Ord, Show, Generic)

atom :: Compression f a => f a -> CompressedSeq f a
atom = RLESequence . FingerTree.singleton

pattern Empty :: Compression f a => CompressedSeq f a
pattern Empty <-
  (FingerTree.null . atoms -> True)
  where
    Empty = RLESequence FingerTree.empty

data VeiwL f a = EmptyL | a :< CompressedSeq f a

viewl :: Compression f a => CompressedSeq f a -> VeiwL f a
viewl (RLESequence s) = case FingerTree.viewl s of
  FingerTree.EmptyL -> EmptyL
  x FingerTree.:< xs -> case popHead x of
    (a, Nothing) -> a :< RLESequence xs
    (a, Just x') -> a :< (atom x' <> RLESequence xs)

pattern (:<|) :: Compression f a => a -> CompressedSeq f a -> CompressedSeq f a
pattern x :<| xs <-
  (viewl -> x :< xs)
  where
    x :<| xs = singleton x <> xs

data VeiwR f a = EmptyR | CompressedSeq f a :> a

viewr :: Compression f a => CompressedSeq f a -> VeiwR f a
viewr (RLESequence s) = case FingerTree.viewr s of
  FingerTree.EmptyR -> EmptyR
  xs FingerTree.:> x -> case popTail x of
    (Nothing, a) -> RLESequence xs :> a
    (Just x', a) -> (RLESequence xs <> atom x') :> a

pattern (:|>) :: Compression f a => CompressedSeq f a -> a -> CompressedSeq f a
pattern xs :|> x <-
  (viewr -> xs :> x)
  where
    xs :|> x = xs <> singleton x

{-# COMPLETE Empty, (:|>) #-}

{-# COMPLETE Empty, (:<|) #-}

instance Compression f a => Semigroup (CompressedSeq f a) where
  RLESequence xs <> RLESequence ys =
    case (FingerTree.viewr xs, FingerTree.viewl ys) of
      (FingerTree.EmptyR, _) -> RLESequence ys
      (_, FingerTree.EmptyL) -> RLESequence xs
      (xxs FingerTree.:> x, y FingerTree.:< yys) -> case tryConcat x y of
        -- Recursion terminates because there is one fewer total element.
        Just x' -> RLESequence xxs <> atom x' <> RLESequence yys
        Nothing -> RLESequence (xs <> ys)

instance Compression f a => Monoid (CompressedSeq f a) where
  mempty = Empty

instance Foldable f => Foldable (CompressedSeq f) where
  foldMap f (RLESequence xs) = foldMap (foldMap f) xs

singleton :: Compression f a => a -> CompressedSeq f a
singleton x = atom (solo x)

fromList :: Compression f a => [a] -> CompressedSeq f a
fromList xs = mconcat (singleton <$> xs)

null :: CompressedSeq f a -> Bool
null (RLESequence t) = FingerTree.null t

replicate :: Compression f a => Int -> a -> CompressedSeq f a
replicate n = fromList . List.replicate n

replicateM ::
  (Applicative m, Compression f a) => Int -> m a -> m (CompressedSeq f a)
replicateM n m = fromList <$> Monad.replicateM n m

length :: Compression f a => CompressedSeq f a -> Int
length (RLESequence xs) = getLength (measure xs)

splitAt ::
  Compression f a =>
  Int ->
  CompressedSeq f a ->
  (CompressedSeq f a, CompressedSeq f a)
splitAt n (RLESequence xs) = case FingerTree.split (> Length n) xs of
  (a, FingerTree.viewl -> x FingerTree.:< b) ->
    case trySplit x (n - getLength (measure a)) of
      This x' -> (RLESequence a <> atom x', RLESequence b)
      That y' -> (RLESequence a, atom y' <> RLESequence b)
      These x' y' -> (RLESequence a <> atom x', atom y' <> RLESequence b)
  _ -> (RLESequence xs, Empty)

lookup :: Compression f a => Int -> CompressedSeq f a -> Maybe a
lookup n xs = case drop n xs of
  Empty -> Nothing
  x :<| _ -> Just x

adjust ::
  Compression f a => (a -> a) -> Int -> CompressedSeq f a -> CompressedSeq f a
adjust f n xs = case splitAt n xs of
  (a, x :<| ys) -> a <> f x :<| ys
  _ -> xs

update :: Compression f a => Int -> a -> CompressedSeq f a -> CompressedSeq f a
update n x = adjust (const x) n

take :: Compression f a => Int -> CompressedSeq f a -> CompressedSeq f a
take n xs = fst (splitAt n xs)

drop :: Compression f a => Int -> CompressedSeq f a -> CompressedSeq f a
drop n xs = snd (splitAt n xs)

insertAt ::
  Compression f a => Int -> a -> CompressedSeq f a -> CompressedSeq f a
insertAt n x xs = a <> singleton x <> b where (a, b) = splitAt n xs

deleteAt :: Compression f a => Int -> CompressedSeq f a -> CompressedSeq f a
deleteAt n xs = case splitAt n xs of
  (a, _ :<| ys) -> a <> ys
  _ -> xs
