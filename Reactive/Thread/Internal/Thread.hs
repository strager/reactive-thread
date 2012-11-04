{-# LANGUAGE GADTs #-}

module Reactive.Thread.Internal.Thread
  ( Thread
  , fork
  , yield
  , runThread
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Parallel
import Control.Monad.Trans.Class

import Reactive.Thread.Internal.VarSource

-- | A thread of execution.
data Thread v o m a where
  Pure :: a -> Thread v o m a
  Lift :: m (Thread v o m a) -> Thread v o m a
  Yield :: o -> Thread v o m a -> Thread v o m a

  Fork
    :: Thread v b m ()  -- ^ Thread computation.
    -> v b              -- ^ Output variable.
    -> Thread v o m a   -- ^ Rest.
    -> Thread v o m a

instance (Monad m) => Monad (Thread v o m) where
  return = Pure

  Pure x >>= f = f x
  Lift m >>= f = Lift (m >>= \ t -> return (t >>= f))
  Yield x m >>= f = Yield x (m >>= f)
  Fork thread var m >>= f = Fork thread var (m >>= f)

instance (Functor m) => Functor (Thread v o m) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Lift m) = Lift $ fmap (fmap f) m
  fmap f (Yield x m) = Yield x $ fmap f m
  fmap f (Fork thread var m) = Fork thread var $ fmap f m

instance MonadTrans (Thread v o) where
  lift m = Lift (liftM Pure m)

instance (MonadIO m) => MonadIO (Thread v o m) where
  liftIO = lift . liftIO

-- | Creates a new thread which executes in parallel,
-- returning the forked thread's output variable.
fork
  :: (Functor m, Monad m, NewVar v m)
  => b                   -- ^ Initial value.
  -> Thread v b m a      -- ^ Computation to fork.
  -> Thread v o m (v b)  -- ^ Var.
fork z m = do
  var <- lift $ newVar z
  Fork (void m) var (Pure var)

-- | Yields a value to this thread's output variable.
yield :: (Monad m) => o -> Thread v o m ()
yield x = Yield x (return ())

-- | Executes a thread in the parent monad.
runThread
  :: ( MonadFork m
     , WriteVar v m
     )
  => v o             -- ^ Output variable.
  -> Thread v o m a
  -> m a
runThread output = go
  where
    go (Pure x) = return x
    go (Lift m) = m >>= go
    go (Yield x m) = writeVar output x >> go m
    go (Fork thread var m)
      = forkExec (runThread var thread) >> go m
