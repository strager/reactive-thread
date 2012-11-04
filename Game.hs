module Game where

import Control.Concurrent (threadDelay)
import Data.Maybe
import Control.Monad.Trans

import qualified Control.Monad

import Reactive.Thread

-- Stolen from Network.CGI.Protocol in package 'cgi'.
-- BSD license, (c) Bjorn Bringert 2006.
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

readCurrentNumber :: Update Int ()
readCurrentNumber = forever $ do
  line <- liftIO getLine
  case maybeRead line of
    Just number -> yield number
    Nothing -> return ()

timer :: Int -> Update () ()
timer us = Control.Monad.forever
  $ liftIO (threadDelay us) >> yield ()

gameLoop :: Update () ()
gameLoop = do
  curNumber <- parallel 0 readCurrentNumber
  tick <- parallel () $ timer (1 * 1000 * 1000)
  forever $ do
    _ <- query tick
    num <- query curNumber
    liftIO $ putStrLn $ "You typed the number: " ++ show num

main :: IO ()
main = runUpdate gameLoop
