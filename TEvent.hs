module TEvent
  ( TEvent
  , newTEvent
  , newFiredTEvent
  , fireTEvent
  , blockTEvent
  ) where

import Control.Applicative
import Control.Concurrent.STM

-- If 'Nothing', the event has fired.
--
-- If 'Just', contained is a list of handlers to be notified
-- when the event fires.
newtype TEvent = TEvent (TVar (Maybe [TMVar ()]))
  deriving (Eq)

fromMaybe_ :: (Applicative m) => (a -> m ()) -> Maybe a -> m ()
fromMaybe_ = maybe (pure ())

newTEvent :: STM (TEvent)
newTEvent = TEvent <$> newTVar (Just [])

newFiredTEvent :: STM (TEvent)
newFiredTEvent = TEvent <$> newTVar Nothing

-- | Wakes up all listeners blocking (via 'blockTEvent') on
-- the event.
fireTEvent :: TEvent -> STM ()
fireTEvent (TEvent event)
  = swapTVar event Nothing >>= fromMaybe_
    (mapM_ $ \ var -> putTMVar var ())

-- | Waits for an event to be fired.
blockTEvent :: TEvent -> STM ()
blockTEvent (TEvent event)
  = readTVar event >>= fromMaybe_ (\ listeners -> do
      myListener <- newEmptyTMVar
      writeTVar event $ Just (myListener : listeners)
      takeTMVar myListener)
