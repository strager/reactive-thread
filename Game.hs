module Game where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Thread
import IOThread
import TEventVar
import VarSource

type T o = Thread TEventVar o IOThread

-- Stolen from Network.CGI.Protocol in package 'cgi'.
-- BSD license, (c) Bjorn Bringert 2006.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

readCurrentNumber :: T Int ()
readCurrentNumber = forever $ do
  line <- liftIO getLine
  case maybeRead line of
    Just number -> yield number
    Nothing -> return ()

gameLoop :: T () ()
gameLoop = do
  curNumber <- fork 0 readCurrentNumber
  forever $ do
    lift blockRead
    num <- lift $ readVar curNumber
    liftIO $ putStrLn $ "You typed the number: " ++ show num

main :: IO ()
main = runIOThread $ do
  var <- newVar ()
  runThread var gameLoop
