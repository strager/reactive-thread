module TEvent
  ( TEvent
  , fire
  , wait
  ) where

import Control.Applicative
import Control.Concurrent.STM

-- If 'Nothing', the event has fired.
--
-- If 'Just', contained is a list of handlers to be notified
-- when the event fires.
newtype TEvent = TEvent (TVar (Maybe [TMVar ()]))

readWriteTVar :: TVar a -> a -> STM a
readWriteTVar var x = readTVar var <* writeTVar var x

fromMaybe_ :: (Applicative m) => (a -> m ()) -> Maybe a -> m ()
fromMaybe_ = maybe (pure ())

-- | Fires an event, notifying all blocked listeners.
fire :: TEvent -> STM ()
fire (TEvent event)
  = readWriteTVar event Nothing >>= fromMaybe_
    (mapM_ $ \ var -> putTMVar var ())

-- | Waits for an event to be fired.
wait :: TEvent -> STM ()
wait (TEvent event)
  = readTVar event >>= fromMaybe_ (\ listeners -> do
      myListener <- newEmptyTMVar
      writeTVar event $ Just (myListener : listeners)
      takeTMVar myListener)
