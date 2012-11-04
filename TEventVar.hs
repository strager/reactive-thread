module TEventVar
  ( TEventVar
  , newTEventVar
  , readTEventVar
  , writeTEventVar
  ) where

import Control.Applicative
import Control.Concurrent.STM

import TEvent

-- | A var which fires an event when modified.
newtype TEventVar a = TEventVar (TVar (a, TEvent))
  deriving (Eq)

newTEventVar :: a -> STM (TEventVar a)
newTEventVar x = do
  event <- newTEvent
  TEventVar <$> newTVar (x, event)

-- | Reads the value of a 'TEventVar', yielding an event
-- which fires when the 'TEventVar' is modified.
readTEventVar :: TEventVar a -> STM (a, TEvent)
readTEventVar (TEventVar var) = readTVar var

-- | Writes the value of a 'TEventVar', notifying readers
-- that the var has been modified.
writeTEventVar :: TEventVar a -> a -> STM TEvent
writeTEventVar (TEventVar var) x = do
  event <- newTEvent
  (_old, oldEvent) <- swapTVar var (x, event)
  fireTEvent oldEvent
  return event
