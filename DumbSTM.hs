{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module DumbSTM
  ( DumbSTMVar
  , DumbSTM
  , runDumbSTM

  , newDumbSTMVar
  , readDumbSTMVar
  , writeDumbSTMVar

  , blockRead
  ) where

import Any
import TEvent
import TEventVar
import VarSource

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Parallel
import Control.Monad.State
import Data.List

newtype DumbSTMVar a = DumbSTMVar
  { unDumbSTMVar :: TEventVar a }
  deriving (Eq)

newtype VarCache = VarCache [CacheEntry]

data CacheEntry = CacheEntry
  { cacheToken :: !(Any1 DumbSTMVar)
  , cacheUpdateEvent :: !TEvent
  , cacheValue :: Any0
  }

entryIsVar :: forall a. DumbSTMVar a -> CacheEntry -> Bool
entryIsVar var entry = var == any1 (cacheToken entry)

emptyCache :: VarCache
emptyCache = VarCache []

unCache :: (Monad m) => DumbSTMVar a -> StateT VarCache m ()
unCache var = modify $ \ (VarCache entries) -> VarCache
  $ filter (not . entryIsVar var) entries

addCache
  :: (Monad m)
  => DumbSTMVar a
  -> a
  -> TEvent
  -> StateT VarCache m ()
addCache var x event
  = modify $ \ (VarCache entries)
    -> VarCache $ cacheEntry : entries
  where
    cacheEntry = CacheEntry
      { cacheToken = Any1 var
      , cacheUpdateEvent = event
      , cacheValue = Any0 x
      }

readCacheEntry
  :: (Monad m)
  => DumbSTMVar a
  -> StateT VarCache m (Maybe CacheEntry)
readCacheEntry var = do
  VarCache vars <- get
  return $ find (entryIsVar var) vars

readCacheValue
  :: (Monad m)
  => DumbSTMVar a
  -> StateT VarCache m (Maybe a)
readCacheValue
  = liftM (fmap (any0 . cacheValue))
  . readCacheEntry

cachedEvents
  :: (Monad m)
  => StateT VarCache m [TEvent]
cachedEvents = do
  VarCache vars <- get
  return $ map cacheUpdateEvent vars

newtype DumbSTM a = DumbSTM (StateT VarCache IO a)
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadIO
    )

instance MonadParallel DumbSTM where
  bindM2 = error "bindM2: TODO"

instance MonadFork DumbSTM where
  forkExec m = do
    joiner <- liftIO $ forkExec (runDumbSTM m)
    return $ liftIO joiner

runDumbSTM :: DumbSTM a -> IO a
runDumbSTM (DumbSTM m) = evalStateT m emptyCache

newDumbSTMVar :: a -> DumbSTM (DumbSTMVar a)
newDumbSTMVar
  = DumbSTM . liftIO . atomically
  . liftM DumbSTMVar . newTEventVar

readDumbSTMVar :: DumbSTMVar a -> DumbSTM a
readDumbSTMVar var = DumbSTM $ do
  mX <- readCacheValue var
  case mX of
    Just x -> return x
    Nothing -> do
      (x, event) <- liftIO . atomically
        . readTEventVar $ unDumbSTMVar var
      addCache var x event
      return x

writeDumbSTMVar :: DumbSTMVar a -> a -> DumbSTM ()
writeDumbSTMVar var x = DumbSTM $ do
  unCache var
  event <- liftIO . atomically $ do
    _ <- writeTEventVar (unDumbSTMVar var) x
    newFiredTEvent
  addCache var x event

foldl1Default
  :: a -> (a -> a -> a) -> [a] -> a
foldl1Default _ f (x:xs) = foldl f x xs
foldl1Default d _ [] = d

blockRead :: DumbSTM ()
blockRead = DumbSTM $ do
  events <- cachedEvents
  put emptyCache
  liftIO . atomically
    . foldl1Default (return ()) orElse
    $ map blockTEvent events

instance NewVar DumbSTMVar DumbSTM where
  newVar = newDumbSTMVar

instance WriteVar DumbSTMVar DumbSTM where
  writeVar = writeDumbSTMVar
