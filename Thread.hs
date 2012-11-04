{-# LANGUAGE GADTs #-}

module Thread where

import Control.Monad
import Control.Monad.Parallel
import Control.Concurrent (forkIO)
import Control.Monad.Trans
import System.IO.Unsafe

import VarSource

data Thread v i o m a where
  Pure :: a -> Thread v i o m a
  Lift :: m (Thread v i o m a) -> Thread v i o m a

  Yield
    :: o                 -- ^ Value to yield.
    -> Thread v i o m a  -- ^ Rest.
    -> Thread v i o m a

  Fork
    :: Thread v i b m ()  -- ^ Thread computation.
    -> v b                -- ^ Output variable.
    -> Thread v i o m a   -- ^ Rest.
    -> Thread v i o m a

  Read
    :: v b                      -- ^ Var to read.
    -> (b -> Thread v i o m a)  -- ^ Rest.
    -> Thread v i o m a

  Block
    :: [AnyVar v]        -- ^ Vars to block on.
    -> Thread v i o m a  -- ^ Rest.
    -> Thread v i o m a

instance (Monad m) => Monad (Thread v i o m) where
  return = Pure

  Pure x >>= f = f x
  Lift m >>= f = Lift (m >>= \ t -> return (t >>= f))
  Yield x m >>= f = Yield x (m >>= f)
  Fork thread var m >>= f = Fork thread var (m >>= f)
  Read var m >>= f = Read var (m >=> f)
  Block vars m >>= f = Block vars (m >>= f)

instance (Functor m) => Functor (Thread v i o m) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Lift m) = Lift $ fmap (fmap f) m
  fmap f (Yield x m) = Yield x $ fmap f m
  fmap f (Fork thread var m) = Fork thread var $ fmap f m
  fmap f (Read var m) = Read var $ fmap f . m
  fmap f (Block vars m) = Block vars $ fmap f m

instance MonadTrans (Thread v i o) where
  lift m = Lift (liftM Pure m)

fork
  :: (Functor m, Monad m, NewVar v m)
  => b                     -- ^ Initial value.
  -> Thread v i b m a      -- ^ Computation to fork.
  -> Thread v i o m (v b)  -- ^ Var.
fork z m = do
  var <- lift $ newVar z
  Fork (void m) var (Pure var)

runThread
  :: ( MonadFork m
     , NewVar v m
     , WriteVar v m
     , ReadVar v m
     , BlockVars v m
     )
  => v o             -- ^ Output variable.
  -> Thread v i o m a
  -> m a
runThread output = go
  where
    go (Pure x) = return x
    go (Lift m) = m >>= go
    go (Yield x m) = writeVar output x >> go m
    go (Fork thread var m)
      = forkExec (runThread var thread) >> go m
    go (Read var f) = readVar var >>= go . f
    go (Block vars m) = blockVars vars >> go m
