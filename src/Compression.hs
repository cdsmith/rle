{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compression where

import Data.Kind (Type)

class Foldable f => Compression (f :: Type -> Type) a where
  count :: f a -> Int
  solo :: a -> f a
  popHead :: f a -> (a, [f a])
  popTail :: f a -> ([f a], a)
  tryMerge :: f a -> f a -> Maybe (f a)
  split :: f a -> Int -> ([f a], [f a])

-- LAWS: TODO

-- If
--    tryMerge a b == Just ab
--    tryMerge b c == Just bc
-- Then
--    tryMerge a bc == tryMerge ab c /= Nothing
