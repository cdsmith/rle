{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compression where

import Data.Kind (Type)
import Data.These (These)

class Compression (f :: Type -> Type) a where
  logicalLength :: f a -> Int
  solo :: a -> f a
  popHead :: f a -> (a, Maybe (f a))
  popTail :: f a -> (Maybe (f a), a)
  tryConcat :: f a -> f a -> Maybe (f a)
  trySplit :: f a -> Int -> These (f a) (f a)
