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
    atom,
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

import Compression (Compression (..))
import qualified Control.Monad as Monad
import Data.Coerce (coerce)
import Data.FingerTree (FingerTree, Measured (..))
import qualified Data.FingerTree as FingerTree
import Data.Foldable (toList)
import qualified Data.List as List
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

newtype Length = Length {getLength :: Int} deriving (Eq, Ord, Show)

instance Semigroup Length where
  Length x <> Length y = Length (x + y)

instance Monoid Length where
  mempty = Length 0

newtype Atom a = Atom {getAtom :: a} deriving (Eq, Ord, Show, Generic)

instance Compression f a => Measured Length (Atom (f a)) where
  measure = Length . count . getAtom

newtype CompressedSeq f a = CompressedSeq
  { atomTree :: FingerTree Length (Atom (f a))
  }
  deriving stock (Eq, Ord, Show, Generic)

atom :: Compression f a => f a -> CompressedSeq f a
atom = CompressedSeq . FingerTree.singleton . Atom

atoms :: Compression f a => CompressedSeq f a -> [f a]
atoms = coerce . toList . atomTree

pattern Empty :: Compression f a => CompressedSeq f a
pattern Empty <-
  (FingerTree.null . atomTree -> True)
  where
    Empty = CompressedSeq FingerTree.empty

data ViewL f a = EmptyL | a :< CompressedSeq f a

viewl :: Compression f a => CompressedSeq f a -> ViewL f a
viewl (CompressedSeq s) = case FingerTree.viewl s of
  FingerTree.EmptyL -> EmptyL
  Atom x FingerTree.:< xs ->
    let (a, as) = popHead x
     in a :< (foldMap atom as <> CompressedSeq xs)

pattern (:<|) :: Compression f a => a -> CompressedSeq f a -> CompressedSeq f a
pattern x :<| xs <-
  (viewl -> x :< xs)
  where
    x :<| xs = singleton x <> xs

data ViewR f a = EmptyR | CompressedSeq f a :> a

viewr :: Compression f a => CompressedSeq f a -> ViewR f a
viewr (CompressedSeq s) = case FingerTree.viewr s of
  FingerTree.EmptyR -> EmptyR
  xs FingerTree.:> Atom x ->
    let (as, a) = popTail x
     in (CompressedSeq xs <> foldMap atom as) :> a

pattern (:|>) :: Compression f a => CompressedSeq f a -> a -> CompressedSeq f a
pattern xs :|> x <-
  (viewr -> xs :> x)
  where
    xs :|> x = xs <> singleton x

{-# COMPLETE Empty, (:|>) #-}

{-# COMPLETE Empty, (:<|) #-}

instance Compression f a => Semigroup (CompressedSeq f a) where
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

singleton :: Compression f a => a -> CompressedSeq f a
singleton x = atom (solo x)

fromList :: Compression f a => [a] -> CompressedSeq f a
fromList xs = mconcat (singleton <$> xs)

null :: CompressedSeq f a -> Bool
null (CompressedSeq t) = FingerTree.null t

replicate :: Compression f a => Int -> a -> CompressedSeq f a
replicate n = fromList . List.replicate n

replicateM ::
  (Applicative m, Compression f a) => Int -> m a -> m (CompressedSeq f a)
replicateM n m = fromList <$> Monad.replicateM n m

length :: Compression f a => CompressedSeq f a -> Int
length (CompressedSeq xs) = getLength (measure xs)

splitAt ::
  Compression f a =>
  Int ->
  CompressedSeq f a ->
  (CompressedSeq f a, CompressedSeq f a)
splitAt n (CompressedSeq xs) = case FingerTree.split (> Length n) xs of
  (a, FingerTree.viewl -> Atom x FingerTree.:< b) ->
    let (x1, x2) = split x (n - getLength (measure a))
     in (CompressedSeq a <> foldMap atom x1, foldMap atom x2 <> CompressedSeq b)
  _ -> (CompressedSeq xs, Empty)

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
