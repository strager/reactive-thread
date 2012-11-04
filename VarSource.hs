{-# LANGUAGE MultiParamTypeClasses #-}

module VarSource
  ( NewVar(..)
  , WriteVar(..)
  ) where

import Data.IORef

class (Monad m) => NewVar v m where
  newVar :: a -> m (v a)
class (Monad m) => WriteVar v m where
  writeVar :: v a -> a -> m ()

instance NewVar IORef IO where
  newVar = newIORef
instance WriteVar IORef IO where
  writeVar = writeIORef
