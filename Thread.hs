module Thread where

import Control.Monad
import Control.Monad.Trans

data Thread i o m a
  = Pure a
  | Lift (m (Thread i o m a))

  | Await (i -> Thread i o m a)
  | Yield o (Thread i o m a)

instance (Monad m) => Monad (Thread i o m) where
  return = Pure

  Pure x >>= f = f x
  Lift m >>= f = Lift (m >>= \ t -> return (t >>= f))
  Await cont >>= f = Await (\ k -> cont k >>= f)
  Yield x m >>= f = Yield x (m >>= f)

instance MonadTrans (Thread i o) where
  lift m = Lift (liftM Pure m)

runThread :: (Read i, Show o) => Thread i o IO a -> IO a
runThread (Pure x)
  = return x

runThread (Lift m)
  = m >>= runThread

runThread (Await cont)
  = liftM read getLine >>= runThread . cont

runThread (Yield x m)
  = print x >> runThread m
