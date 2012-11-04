module Game where

-- import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent hiding (yield)
import Control.Monad hiding (forever)
import Control.Monad.Trans
import Data.List

import qualified Graphics.UI.SDL as SDL

import Reactive.Thread

data UpOrDown = Up | Down
  deriving (Eq, Ord, Show)

type KeyboardChan = Chan (UpOrDown, SDL.Keysym)

heldOf :: KeyboardChan -> [SDL.SDLKey] -> Update o (UpdateVar [SDL.SDLKey])
heldOf origKeyChan keys = parallel [] $ do
  keyChan <- liftIO $ dupChan origKeyChan
  forever $ go keyChan []
  where
    go keyChan held = do
      (upOrDown, sym) <- liftIO $ readChan keyChan
      let key = SDL.symKey sym
      when (key `elem` keys) $ do
        let held' = case upOrDown of
              Up -> delete key held
              Down -> key : held
        yield held'
        go keyChan held'

keyboard
  :: KeyboardChan
  -> a
  -> [(SDL.SDLKey, a)]
  -> Update o (UpdateVar a)
keyboard keyChan z keys = parallel z $ do
  held <- heldOf keyChan (map fst keys)

  forever $ do
    heldKeys <- query held
    case heldKeys of
      [] -> yield z
      (key:_) -> maybe (return ()) yield $ lookup key keys

type Thread o = Update o ()

data Sprite = Sprite !Int !Int
  deriving (Show)

player :: KeyboardChan -> Update o (UpdateVar Sprite)
player keyChan = parallel (Sprite 0 0) $ do
  x <- keyboard keyChan 0 [(SDL.SDLK_LEFT, -1), (SDL.SDLK_RIGHT, 1)]
  y <- keyboard keyChan 0 [(SDL.SDLK_DOWN, -1), (SDL.SDLK_UP, 1)]
  forever $ do
    sprite <- Sprite
      `liftM` query x
      `ap` query y
    yield sprite

gameLoop :: KeyboardChan -> Thread ()
gameLoop keyChan = do
  p <- player keyChan
  forever $ do
    sprite <- query p
    liftIO $ print sprite

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  _surface <- SDL.setVideoMode initialWidth initialHeight 32
    [SDL.HWSurface, SDL.Resizable, SDL.RLEAccel, SDL.DoubleBuf]

  keyboardChan <- newChan
  gameThreadID <- forkIO $ runUpdate (gameLoop keyboardChan)
  _ <- eventLoop keyboardChan
  killThread gameThreadID

  where
    initialWidth = 640
    initialHeight = 480

    eventLoop keyboardChan = pollEvent
      where
        pollEvent = SDL.pollEvent >>= handleEvent

        handleEvent event = case event of
          SDL.NoEvent -> SDL.waitEvent >>= handleEvent
          SDL.KeyDown sym -> do
            writeChan keyboardChan (Down, sym)
            pollEvent
          SDL.KeyUp sym -> do
            writeChan keyboardChan (Up, sym)
            pollEvent
          _ -> pollEvent
