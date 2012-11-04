{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Update
  ( Update
  , runUpdate

  , UpdateVar
  , parallel
  , query
  , yield

  , forever
  ) where

-- import Control.Applicative
import Control.Monad hiding (forever)
import Control.Monad.Trans

import qualified Control.Monad

import DumbSTM

import qualified Thread

newtype Update o a = Update
  { unUpdate :: Thread.Thread DumbSTMVar o DumbSTM a }
  deriving
    ( Monad
    , Functor
    -- , Applicative
    , MonadIO
    )

newtype UpdateVar a = UpdateVar
  { unUpdateVar :: DumbSTMVar a }

parallel 
  :: b
  -> Update b a
  -> Update o (UpdateVar b)
parallel z m = Update . liftM UpdateVar
  $ Thread.fork z (unUpdate m)

yield :: o -> Update o ()
yield = Update . Thread.yield

query :: UpdateVar a -> Update o a
query = Update . lift . readDumbSTMVar . unUpdateVar

forever :: Update o a -> Update o b
forever m = Control.Monad.forever
  $ Update (lift blockRead) >> m

runUpdate :: Update () a -> IO a
runUpdate m = runDumbSTM $ do
  var <- newDumbSTMVar ()
  Thread.runThread var (unUpdate m)
