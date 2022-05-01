{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module CompressedSeq
  ( CompressedSeq (Empty, (:<|), (:|>)),
    atom,
    atoms,
    singleton,
    fromList,
    replicate,
    replicateM,
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

import Compression (Compression (..))
import qualified Control.Monad as Monad
import Data.Coerce (coerce)
import Data.FingerTree (FingerTree, Measured (..))
import qualified Data.FingerTree as FingerTree
import Data.Foldable (Foldable (..))
import qualified Data.List as List
import Prelude
  ( Applicative (..),
    Bool (..),
    Eq (..),
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

newtype Length = Length {getLength :: Int} deriving (Eq, Ord, Show)

instance Semigroup Length where
  Length x <> Length y = Length (x + y)

instance Monoid Length where
  mempty = Length 0

newtype Atom a = Atom {getAtom :: a} deriving (Eq, Ord, Show)

instance Compression f a => Measured Length (Atom (f a)) where
  measure = Length . length . getAtom

data CompressedSeq f a where
  CompressedSeq ::
    Compression f a =>
    {atomTree :: FingerTree Length (Atom (f a))} ->
    CompressedSeq f a

deriving instance Eq (f a) => Eq (CompressedSeq f a)

deriving instance Ord (f a) => Ord (CompressedSeq f a)

deriving instance Show (f a) => Show (CompressedSeq f a)

atom :: Compression f a => f a -> CompressedSeq f a
atom = CompressedSeq . FingerTree.singleton . Atom

atoms :: CompressedSeq f a -> [f a]
atoms = coerce . toList . atomTree

pattern Empty :: Compression f a => CompressedSeq f a
pattern Empty <-
  (FingerTree.null . atomTree -> True)
  where
    Empty = CompressedSeq FingerTree.empty

data ViewL f a = EmptyL | a :< CompressedSeq f a

viewl :: CompressedSeq f a -> ViewL f a
viewl (CompressedSeq s) = case FingerTree.viewl s of
  FingerTree.EmptyL -> EmptyL
  Atom x FingerTree.:< xs ->
    let (a, as) = popHead x
     in a :< (foldMap atom as <> CompressedSeq xs)

pattern (:<|) :: a -> CompressedSeq f a -> CompressedSeq f a
pattern x :<| xs <-
  (viewl -> x :< xs)
  where
    x :<| CompressedSeq xs = singleton x <> CompressedSeq xs

data ViewR f a = EmptyR | CompressedSeq f a :> a

viewr :: CompressedSeq f a -> ViewR f a
viewr (CompressedSeq s) = case FingerTree.viewr s of
  FingerTree.EmptyR -> EmptyR
  xs FingerTree.:> Atom x ->
    let (as, a) = popTail x
     in (CompressedSeq xs <> foldMap atom as) :> a

pattern (:|>) :: CompressedSeq f a -> a -> CompressedSeq f a
pattern xs :|> x <-
  (viewr -> xs :> x)
  where
    CompressedSeq xs :|> x = CompressedSeq xs <> singleton x

{-# COMPLETE Empty, (:|>) #-}

{-# COMPLETE Empty, (:<|) #-}

instance Semigroup (CompressedSeq f a) where
  CompressedSeq xs <> CompressedSeq ys =
    case (FingerTree.viewr xs, FingerTree.viewl ys) of
      (FingerTree.EmptyR, _) -> CompressedSeq ys
      (_, FingerTree.EmptyL) -> CompressedSeq xs
      (xxs FingerTree.:> Atom x, Atom y FingerTree.:< yys) ->
        case tryMerge x y of
          -- Recursion terminates because there is one fewer total element.
          Just x' -> CompressedSeq xxs <> atom x' <> CompressedSeq yys
          Nothing -> CompressedSeq (xs <> ys)

instance Compression f a => Monoid (CompressedSeq f a) where
  mempty = Empty

instance Foldable f => Foldable (CompressedSeq f) where
  foldMap f (CompressedSeq xs) = foldMap (foldMap f . getAtom) xs
  length (CompressedSeq xs) = getLength (measure xs)

singleton :: Compression f a => a -> CompressedSeq f a
singleton x = atom (solo x)

fromList :: Compression f a => [a] -> CompressedSeq f a
fromList xs = mconcat (singleton <$> xs)

replicate :: Compression f a => Int -> a -> CompressedSeq f a
replicate n = fromList . List.replicate n

replicateM ::
  (Applicative m, Compression f a) => Int -> m a -> m (CompressedSeq f a)
replicateM n m = fromList <$> Monad.replicateM n m

splitAt :: Int -> CompressedSeq f a -> (CompressedSeq f a, CompressedSeq f a)
splitAt n (CompressedSeq xs) = case FingerTree.split (> Length n) xs of
  (a, FingerTree.viewl -> Atom x FingerTree.:< b) ->
    let (x1, x2) = split x (n - getLength (measure a))
     in (CompressedSeq a <> foldMap atom x1, foldMap atom x2 <> CompressedSeq b)
  _ -> (CompressedSeq xs, Empty)

lookup :: Int -> CompressedSeq f a -> Maybe a
lookup n (CompressedSeq xs) = case drop n (CompressedSeq xs) of
  Empty -> Nothing
  x :<| _ -> Just x

adjust :: (a -> a) -> Int -> CompressedSeq f a -> CompressedSeq f a
adjust f n xs = case splitAt n xs of
  (a, x :<| ys) -> a <> f x :<| ys
  _ -> xs

update :: Int -> a -> CompressedSeq f a -> CompressedSeq f a
update n x = adjust (const x) n

take :: Int -> CompressedSeq f a -> CompressedSeq f a
take n xs = fst (splitAt n xs)

drop :: Int -> CompressedSeq f a -> CompressedSeq f a
drop n xs = snd (splitAt n xs)

insertAt :: Int -> a -> CompressedSeq f a -> CompressedSeq f a
insertAt n x (CompressedSeq xs) = a <> singleton x <> b
  where
    (a, b) = splitAt n (CompressedSeq xs)

deleteAt :: Int -> CompressedSeq f a -> CompressedSeq f a
deleteAt n xs = case splitAt n xs of
  (a, _ :<| ys) -> a <> ys
  _ -> xs
