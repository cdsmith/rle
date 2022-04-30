{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MultiCompress () where

import Compression (Compression (..))
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (..))
import Data.Monoid (Sum (..))

instance (Compression f (g a), Compression g a) => Compression (Compose f g) a where
  count (Compose xs) =
    getSum (foldMap (Sum . count) xs)
  solo = Compose . solo . solo
  popHead (Compose x) = (b, coerce (map solo bs ++ as))
    where
      (a, as) = popHead x
      (b, bs) = popHead a
  popTail (Compose x) = (coerce (as ++ map solo bs), b)
    where
      (as, a) = popTail x
      (bs, b) = popTail a
  tryMerge (Compose x) (Compose y) = Nothing
  split (Compose x) i = single i x
    where
      single i x
        | i <= 0 = ([], [Compose x])
        | otherwise =
            let (a, as) = popHead x
             in if i <= count a
                  then
                    let (a1, a2) = split a i
                     in ( map (Compose . solo) a1,
                          map (Compose . solo) a2 ++ map Compose as
                        )
                  else
                    let i' = i - count a
                        (b1, b2) = multi i' as
                     in (Compose (solo a) : b1, b2)
      multi _ [] = ([], [])
      multi i (x : xs)
        | i < count (Compose x) =
            let (x1, x2) = single i x
             in (x1, x2 ++ map Compose xs)
        | otherwise =
            let i' = i - count x
                (b1, b2) = multi i' xs
             in (Compose x : b1, b2)

-- --------------- x ------------ x :: f (g a)

-- |   a   |        as          | a :: g a, as :: [f (g a)]

{-
123123123123412341234

[Run (Interval 1 3) 3, Run (Interval 1 4) 3]
 123 123 123 1234 1234 1234
[Run (Interval 1 4) 3, Run (Interval 4 4) 1, Run (Interval 1 4) 2]
 123 123 123 123 4 1234 1234

 let xs = 123 123 123 123
 let ys = 4 1234 1234

 xs <> ys = ????
 -}