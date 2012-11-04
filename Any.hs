{-# LANGUAGE GADTs #-}

module Any
  ( Any0(..)
  , any0

  , Any1(..)
  , any1
  ) where

import Unsafe.Coerce

-- | Contains any value.
data Any0 where Any0 :: a -> Any0

-- | Unwraps any value.
--
-- > 'any0' . 'Any0' = 'id'
any0 :: Any0 -> a
any0 (Any0 x) = unsafeCoerce x

-- | Contains any value with kind @* -> *@.
data Any1 k1 where Any1 :: k1 a -> Any1 k1

-- | Unwraps any value with kind @* -> *@.
--
-- > 'any1' . 'Any1' = 'id'
any1 :: Any1 k1 -> k1 a
any1 (Any1 x) = unsafeCoerce x
