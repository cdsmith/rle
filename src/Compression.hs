{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compression where

import Data.Kind (Type)
import Data.These (These)

class Foldable f => Compression (f :: Type -> Type) a where
  logicalLength :: f a -> Int
  solo :: a -> f a
  popHead :: f a -> (a, [f a])
  popTail :: f a -> ([f a], a)
  tryConcat :: f a -> f a -> Maybe (f a)
  trySplit :: f a -> Int -> These (f a) (f a)
