{-# LANGUAGE GADTs #-}

module Any
  ( Any0(..)
  , any0

  , Any1(..)
  , any1
  ) where

import Unsafe.Coerce

data Any0 where Any0 :: a -> Any0
any0 :: Any0 -> a
any0 (Any0 x) = unsafeCoerce x

data Any1 k1 where Any1 :: k1 a -> Any1 k1
any1 :: Any1 k1 -> k1 a
any1 (Any1 x) = unsafeCoerce x
