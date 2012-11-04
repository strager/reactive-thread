{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reactive.Thread.Update
  ( Update
  , runUpdate

  , UpdateVar
  , parallel
  , query
  , yield

  , forever
  , foreverT
  ) where

-- import Control.Applicative
import Control.Monad hiding (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import qualified Control.Monad

import Reactive.Thread.Internal.DumbSTM

import qualified Reactive.Thread.Internal.Thread as Thread

newtype Update o a = Update
  { unUpdate :: Thread.Thread DumbSTMVar o DumbSTM a }
  deriving
    ( Monad
    , Functor
    -- , Applicative
    , MonadIO
    )

-- | A read-only handle to a thread's output variable.
newtype UpdateVar a = UpdateVar
  { unUpdateVar :: DumbSTMVar a }

-- | Executes an 'Update' in parallel, returning a var
-- representing the thread's output variable.
parallel 
  :: b
  -> Update b a
  -> Update o (UpdateVar b)
parallel z m = Update . liftM UpdateVar
  $ Thread.fork z (unUpdate m)

-- | Mutates this thread's output variable.
yield :: o -> Update o ()
yield = Update . Thread.yield

-- | Reads a thread's output variable.
--
-- Reads are cached and tracked; see 'forever'.
query :: UpdateVar a -> Update o a
query = Update . lift . readDumbSTMVar . unUpdateVar

-- | Runs an 'Update' in a loop forever.  Between each loop,
-- execution is blocked until a 'query'ied variable is
-- modified (since it was last read).
forever :: Update o a -> Update o b
forever m = Control.Monad.forever
  $ Update (lift blockRead) >> m

foreverT
  :: (Monad (m (Update o)), MonadTrans m)
  => m (Update o) a -> m (Update o) b
foreverT m = Control.Monad.forever
  $ lift (Update (lift blockRead)) >> m

-- | Runs an 'Update' in the 'IO' monad.
runUpdate :: Update () a -> IO a
runUpdate m = runDumbSTM $ do
  var <- newDumbSTMVar ()
  Thread.runThread var (unUpdate m)
