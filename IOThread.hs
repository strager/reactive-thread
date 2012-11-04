{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module IOThread where

import TEvent
import TEventVar
import Thread
import VarSource

import Control.Monad.Parallel
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent.STM
import Unsafe.Coerce

unAnyVar :: AnyVar v -> v a
unAnyVar (AnyVar x) = unsafeCoerce x

data AnyValue where
  AnyValue :: a -> AnyValue

unAnyValue :: AnyValue -> a
unAnyValue (AnyValue x) = unsafeCoerce x

newtype VarCache = VarCache [CacheEntry]

data CacheEntry = CacheEntry
  { cacheToken :: !(AnyVar TEventVar)
  , cacheUpdateEvent :: !TEvent
  , cacheValue :: AnyValue
  }

entryIsVar :: forall a. TEventVar a -> CacheEntry -> Bool
entryIsVar var entry = var == unAnyVar (cacheToken entry)

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
      { cacheToken = AnyVar var
      , cacheUpdateEvent = event
      , cacheValue = AnyValue x
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
  = liftM (fmap (unAnyValue . cacheValue))
  . readCacheEntry

cachedEvents
  :: (Monad m)
  => StateT VarCache m [TEvent]
cachedEvents = do
  VarCache vars <- get
  return $ map cacheUpdateEvent vars

newtype IOThread a = IOThread (StateT VarCache IO a)
  deriving
    ( Monad
    , Functor
    , Applicative
    , MonadIO
    )

instance MonadParallel IOThread where
  bindM2 = error "bindM2: TODO"

instance MonadFork IOThread where
  forkExec m = do
    joiner <- liftIO $ forkExec (runIOThread m)
    return $ liftIO joiner

runIOThread :: IOThread a -> IO a
runIOThread (IOThread m) = evalStateT m emptyCache

instance NewVar TEventVar IOThread where
  newVar = IOThread . liftIO . atomically . newTEventVar

instance ReadVar TEventVar IOThread where
  readVar var = IOThread $ readCacheValue var >>= \ mX -> case mX of
    Just x -> return x
    Nothing -> do
      (x, event) <- liftIO . atomically $ readTEventVar var
      addCache var x event
      return x

instance WriteVar TEventVar IOThread where
  writeVar var x = IOThread $ do
    unCache var
    event <- liftIO . atomically $ writeTEventVar var x
    addCache var x event

foldl1Default
  :: a -> (a -> a -> a) -> [a] -> a
foldl1Default _ f (x:xs) = foldl f x xs
foldl1Default d _ [] = d

blockRead :: IOThread ()
blockRead = IOThread $ do
  events <- cachedEvents
  put emptyCache
  liftIO . atomically
    . foldl1Default (return ()) orElse
    $ map blockTEvent events
