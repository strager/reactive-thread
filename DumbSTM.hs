{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module DumbSTM where

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

newtype VarCache = VarCache [CacheEntry]

data CacheEntry = CacheEntry
  { cacheToken :: !(Any1 TEventVar)
  , cacheUpdateEvent :: !TEvent
  , cacheValue :: Any0
  }

entryIsVar :: forall a. TEventVar a -> CacheEntry -> Bool
entryIsVar var entry = var == any1 (cacheToken entry)

emptyCache :: VarCache
emptyCache = VarCache []

unCache :: (Monad m) => TEventVar a -> StateT VarCache m ()
unCache var = modify $ \ (VarCache entries) -> VarCache
  $ filter (not . entryIsVar var) entries

addCache
  :: (Monad m)
  => TEventVar a
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
  => TEventVar a
  -> StateT VarCache m (Maybe CacheEntry)
readCacheEntry var = do
  VarCache vars <- get
  return $ find (entryIsVar var) vars

readCacheValue
  :: (Monad m)
  => TEventVar a
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

instance NewVar TEventVar DumbSTM where
  newVar = DumbSTM . liftIO . atomically . newTEventVar

readVar :: TEventVar a -> DumbSTM a
readVar var = DumbSTM $ readCacheValue var >>= \ mX -> case mX of
  Just x -> return x
  Nothing -> do
    (x, event) <- liftIO . atomically $ readTEventVar var
    addCache var x event
    return x

instance WriteVar TEventVar DumbSTM where
  writeVar var x = DumbSTM $ do
    unCache var
    event <- liftIO . atomically $ writeTEventVar var x
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
