{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module VarSource where

import Data.IORef

data AnyVar v where
  AnyVar :: v a -> AnyVar v

class (Monad m) => NewVar v m where
  newVar :: a -> m (v a)
class (Monad m) => ReadVar v m where
  readVar :: v a -> m a
class (Monad m) => WriteVar v m where
  writeVar :: v a -> a -> m ()
class (Monad m) => BlockVars v m where
  blockVars :: [AnyVar v] -> m ()

instance NewVar IORef IO where
  newVar = newIORef
instance ReadVar IORef IO where
  readVar = readIORef
instance WriteVar IORef IO where
  writeVar = writeIORef
