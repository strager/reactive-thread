module Main where

-- import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent hiding (yield)
import Control.Monad hiding (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List

import qualified Graphics.UI.SDL as SDL

import Reactive.Thread

data UpOrDown = Up | Down
  deriving (Eq, Ord, Show)

type KeyboardChan = Chan (UpOrDown, SDL.Keysym)

heldOf :: KeyboardChan -> [SDL.SDLKey] -> Update o (UpdateVar [SDL.SDLKey])
heldOf origKeyChan keys = parallel [] $ do
  keyChan <- liftIO $ dupChan origKeyChan
  evalStateT (foreverT $ go keyChan) []
  where
    go keyChan = do
      (upOrDown, sym) <- liftIO $ readChan keyChan
      let key = SDL.symKey sym
      when (key `elem` keys) $ do
        held <- get
        let held' = case upOrDown of
              Up -> delete key held
              Down -> key : held
        put held'
        lift $ yield held'

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

type Time = Double

constantTick :: Double {-FPS-} -> Update o (UpdateVar Time)
constantTick fps = parallel 0 $ evalStateT (foreverT go) 0
  where
    go = do
      liftIO . threadDelay $ floor (1000 * 1000 / fps)

      t' <- gets succ
      put t'
      lift $ yield t'

velocity
  :: Double            -- ^ Initial position.
  -> UpdateVar Time    -- ^ Time.
  -> UpdateVar Double  -- ^ Velocity.
  -> Update o (UpdateVar Double)
velocity z timeVar velVar = parallel z $ do
  zeroTime <- query timeVar
  evalStateT (foreverT go) (z, zeroTime)
  where
    go = do
      (pos, time) <- get
      time' <- lift $ query timeVar
      vel <- lift $ query velVar

      let dt = time' - time
      pos' <- if dt > 0 && vel /= 0
        then do
          let pos' = pos + dt * vel
          lift $ yield pos'
          return pos'
        else return pos

      put (pos', time')

data Sprite = Sprite !Double !Double
  deriving (Show)

player
  :: UpdateVar Time
  -> KeyboardChan
  -> Update o (UpdateVar Sprite)
player timer keyChan = parallel (Sprite 0 0) $ do
  xVel <- keyboard keyChan 0 [(SDL.SDLK_LEFT, -1), (SDL.SDLK_RIGHT, 1)]
  yVel <- keyboard keyChan 0 [(SDL.SDLK_UP, -1), (SDL.SDLK_DOWN, 1)]
  x <- velocity 0 timer xVel
  y <- velocity 0 timer yVel

  forever $ do
    sprite <- Sprite
      `liftM` query x
      `ap` query y
    yield sprite

gameLoop :: SDL.Surface -> KeyboardChan -> Thread ()
gameLoop surface keyChan = do
  timer <- constantTick 60
  p <- player timer keyChan
  forever $ do
    sprite <- query p

    liftIO $ do
      let format = SDL.surfaceGetPixelFormat surface
      black <- SDL.mapRGB format 0 0 0
      void $ SDL.fillRect surface Nothing black

      drawSprite surface sprite
      SDL.flip surface

drawSprite :: SDL.Surface -> Sprite -> IO ()
drawSprite surface (Sprite x y) = do
  let format = SDL.surfaceGetPixelFormat surface
  color <- SDL.mapRGB format 0 128 255
  void $ SDL.fillRect surface (Just region) color

  where
    region = SDL.Rect
      (floor x) (floor y)
      width height

    width = 32
    height = 32

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  surface <- SDL.setVideoMode initialWidth initialHeight 32
    [SDL.HWSurface, SDL.Resizable, SDL.RLEAccel, SDL.DoubleBuf]

  keyboardChan <- newChan
  gameThreadID <- forkIO $ runUpdate (gameLoop surface keyboardChan)
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
          SDL.KeyDown SDL.Keysym { SDL.symKey = SDL.SDLK_ESCAPE }
            -> return ()  -- Stop.
          SDL.KeyDown sym -> do
            writeChan keyboardChan (Down, sym)
            pollEvent
          SDL.KeyUp sym -> do
            writeChan keyboardChan (Up, sym)
            pollEvent
          _ -> pollEvent
