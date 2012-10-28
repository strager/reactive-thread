{-# LANGUAGE GADTs #-}

module Thread where

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Monad.Trans
import Data.IORef
import System.IO.Unsafe

newtype Var a = Var (IORef a)

data Thread i o m a where
  Pure :: a -> Thread i o m a
  Lift :: m (Thread i o m a) -> Thread i o m a

  Await
    :: (i -> Thread i o m a)  -- ^ Continuation.
    -> Thread i o m a

  Yield
    :: o               -- ^ Value to yield.
    -> Thread i o m a  -- ^ Rest.
    -> Thread i o m a

  Fork
    :: Thread i v m ()       -- ^ Thread computation.
    -> Var v                 -- ^ Output variable.
    -> Thread i o m a        -- ^ Rest.
    -> Thread i o m a

instance (Monad m) => Monad (Thread i o m) where
  return = Pure

  Pure x >>= f = f x
  Lift m >>= f = Lift (m >>= \ t -> return (t >>= f))

  Await cont >>= f = Await (\ k -> cont k >>= f)
  Yield x m >>= f = Yield x (m >>= f)

  Fork thread var m >>= f = Fork thread var (m >>= f)

instance (Functor m) => Functor (Thread i o m) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Lift m) = Lift $ fmap (fmap f) m

  fmap f (Await cont) = Await $ \ k -> fmap f (cont k)
  fmap f (Yield x m) = Yield x $ fmap f m

instance MonadTrans (Thread i o) where
  lift m = Lift (liftM Pure m)

fork :: (Functor m, Monad m) => v -> Thread i v m a -> Thread i o m (Var v)
fork z m = Fork (void m) var (Pure var)
  where
    var = Var $ unsafePerformIO $ newIORef z

runThread
  :: IO i   -- ^ Input source.
  -> Var o  -- ^ Output variable.
  -> Thread i o IO a
  -> IO a
runThread source (Var output) = go
  where
    go (Pure x) = return x
    go (Lift m) = m >>= go
    go (Await cont) = source >>= go . cont
    go (Yield x m) = do
      return $! unsafePerformIO $ {-atomicWriteIORef-} writeIORef output x
      go m
    go (Fork thread var m) = do
      _ <- forkIO $ runThread source var thread
      go m
