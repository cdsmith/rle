{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module RLESequence
  ( RLESequence (Empty, (:<|), (:|>)),
    empty,
    singleton,
    fromList,
    null,
    reverse,
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

import qualified Control.Monad as Monad
import Data.FingerTree (FingerTree, Measured (..))
import qualified Data.FingerTree as FingerTree
import qualified Data.List as List
import GHC.Generics (Generic)
import Prelude
  ( Applicative (..),
    Bool (..),
    Eq (..),
    Foldable (foldMap),
    Int,
    Integral (..),
    Maybe (..),
    Monoid (..),
    Num (..),
    Ord (..),
    Semigroup (..),
    Show (..),
    const,
    fst,
    odd,
    otherwise,
    snd,
    ($),
    (.),
    (<$>),
  )

data Run a = Run a !Int deriving (Eq, Ord, Show)

newtype Length = Length {getLength :: Int} deriving (Eq, Ord, Show, Num)

instance Semigroup Length where
  Length x <> Length y = Length (x + y)

instance Monoid Length where
  mempty = Length 0

instance Measured Length (Run a) where
  measure (Run _ n) = Length n

newtype RLESequence a = RLESequence {rleTree :: FingerTree Length (Run a)}
  deriving stock (Eq, Ord, Show, Generic)

pattern Empty :: RLESequence a
pattern Empty <- (FingerTree.null . rleTree -> True) where Empty = empty

data VeiwL a = EmptyL | a :< RLESequence a

viewl :: RLESequence a -> VeiwL a
viewl (RLESequence s) = case FingerTree.viewl s of
  FingerTree.EmptyL -> EmptyL
  (Run x n FingerTree.:< xs)
    | n == 1 -> x :< RLESequence xs
    | otherwise -> x :< RLESequence (Run x (n - 1) FingerTree.<| xs)

pattern (:<|) :: Eq a => a -> RLESequence a -> RLESequence a
pattern x :<| xs <-
  (viewl -> x :< xs)
  where
    x :<| RLESequence xs = RLESequence $ case FingerTree.viewl xs of
      FingerTree.EmptyL -> FingerTree.singleton (Run x 1)
      Run y n FingerTree.:< ys
        | x == y -> Run y (n + 1) FingerTree.<| ys
        | otherwise -> Run x 1 FingerTree.<| xs

data VeiwR a = EmptyR | RLESequence a :> a

viewr :: RLESequence a -> VeiwR a
viewr (RLESequence s) = case FingerTree.viewr s of
  FingerTree.EmptyR -> EmptyR
  (xs FingerTree.:> Run x n)
    | n == 1 -> RLESequence xs :> x
    | otherwise -> RLESequence (xs FingerTree.|> Run x (n - 1)) :> x

pattern (:|>) :: Eq a => RLESequence a -> a -> RLESequence a
pattern xs :|> x <-
  (viewr -> xs :> x)
  where
    RLESequence xs :|> x = RLESequence $ case FingerTree.viewr xs of
      FingerTree.EmptyR -> FingerTree.singleton (Run x 1)
      ys FingerTree.:> Run y n
        | x == y -> ys FingerTree.|> Run y (n + 1)
        | otherwise -> xs FingerTree.|> Run x 1

instance Eq a => Semigroup (RLESequence a) where
  RLESequence xs <> RLESequence ys = RLESequence $
    case (FingerTree.viewr xs, FingerTree.viewl ys) of
      (FingerTree.EmptyR, _) -> ys
      (_, FingerTree.EmptyL) -> xs
      (xxs FingerTree.:> Run x n, Run y m FingerTree.:< yys)
        | x == y -> (xxs FingerTree.|> Run x (n + m)) <> yys
        | otherwise -> xs <> ys

instance Eq a => Monoid (RLESequence a) where
  mempty = empty

instance Foldable RLESequence where
  foldMap f (RLESequence xs) = foldMap fRun xs
    where
      fRun (Run x n) = go n
        where
          fx = f x
          go 1 = fx
          go n
            | odd n = fx <> go (n - 1)
            | otherwise = let a = go (n `div` 2) in a <> a

empty :: RLESequence a
empty = RLESequence FingerTree.empty

singleton :: a -> RLESequence a
singleton x = RLESequence (FingerTree.singleton (Run x 1))

fromList :: Eq a => [a] -> RLESequence a
fromList xs = RLESequence (FingerTree.fromList runs)
  where
    runs = List.map (\g -> Run (List.head g) (List.length g)) (List.group xs)

null :: RLESequence a -> Bool
null (RLESequence t) = FingerTree.null t

reverse :: RLESequence a -> RLESequence a
reverse (RLESequence s) = RLESequence (FingerTree.reverse s)

replicate :: Int -> a -> RLESequence a
replicate n x = RLESequence (FingerTree.singleton (Run x n))

replicateM :: (Applicative m, Eq a) => Int -> m a -> m (RLESequence a)
replicateM n m = fromList <$> Monad.replicateM n m

length :: RLESequence a -> Int
length (RLESequence xs) = getLength (measure xs)

lookup :: Int -> RLESequence a -> Maybe a
lookup n (RLESequence xs) = case FingerTree.split (> Length n) xs of
  (_, FingerTree.viewl -> Run x _ FingerTree.:< _) -> Just x
  _ -> Nothing

splitAt :: Int -> RLESequence a -> (RLESequence a, RLESequence a)
splitAt n (RLESequence xs) = case FingerTree.split (> Length n) xs of
  (a, FingerTree.viewl -> Run x m FingerTree.:< b) ->
    let i = n - getLength (measure a)
        before
          | i == 0 = a
          | otherwise = a FingerTree.|> Run x i
        after = Run x (m - i) FingerTree.<| b
     in (RLESequence before, RLESequence after)
  _ -> (RLESequence xs, empty)

adjust :: Eq a => (a -> a) -> Int -> RLESequence a -> RLESequence a
adjust f n xs = case splitAt n xs of
  (a, x :<| ys) -> a <> f x :<| ys
  _ -> xs

update :: Eq a => Int -> a -> RLESequence a -> RLESequence a
update n x = adjust (const x) n

take :: Int -> RLESequence a -> RLESequence a
take n xs = fst (splitAt n xs)

drop :: Int -> RLESequence a -> RLESequence a
drop n xs = snd (splitAt n xs)

insertAt :: Eq a => Int -> a -> RLESequence a -> RLESequence a
insertAt n x xs = a <> singleton x <> b where (a, b) = splitAt n xs

deleteAt :: Eq a => Int -> RLESequence a -> RLESequence a
deleteAt n xs = case splitAt n xs of
  (a, x :<| ys) -> a <> ys
  _ -> xs
