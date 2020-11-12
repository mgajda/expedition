{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module SDLContext(Context(..), withContext, screenWidth, screenHeight, initializeContext, freeContext) where

import qualified SDL
import qualified SDL.Mixer                as Mixer
import qualified SDL.Font                 as Font
import qualified SDL.Event                as SDL
import qualified SDL.Image                as Image
import qualified SDL.Input.Keyboard.Codes as SDL
import qualified SDL.Vect                 as SDL
import qualified SDL.Video.Vulkan         as SDL
import qualified Data.Text.IO             as T
import           RIO
import           Lens.Micro.TH
import           Foreign.C.Types(CInt)
import           Control.Restartable.Checkpoint
import           Control.Restartable.Initial
import           Data.Aeson

-- | Initialized SDL context to be passed to global configuration.
data Context = Context {
    ctxWindow    :: SDL.Window
  , ctxRenderer  :: SDL.Renderer
  }

screenWidth  = 1280
screenHeight = 1024

initializeContext :: Integral a
                  => SDL.V2   a -- initial window position
                  -> SDL.V2   a -- initial window size
                  -> IO Context
initializeContext winPos winSize = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Mixer.openAudio Mixer.defaultAudio 256
  ctxWindow    <- SDL.createWindow "AlienExp" SDL.defaultWindow { SDL.windowMode        = SDL.Windowed
                                                                , SDL.windowResizable   = True
                                                                , SDL.windowPosition    = initialWinPos
                                                                , SDL.windowInitialSize = initialWinSize }
  print (initialWinPos, initialWinSize)
  --SDL.windowMinimumSize ctxWindow SDL.$= SDL.V2 screenWidth screenHeight
  ctxRenderer  <- SDL.createRenderer ctxWindow (-1) SDL.defaultRenderer
  return Context {..}
  where
    initialWinPos  | winPos  == SDL.V2 0 0 = SDL.Wherever
                   | otherwise             = SDL.Absolute $ SDL.P $ fmap fromIntegral winPos
    initialWinSize | winSize == SDL.V2 0 0 = SDL.V2 screenWidth screenHeight
                   | otherwise             = fmap fromIntegral winSize

freeContext :: Context -> IO ()
freeContext Context {..} = do
  SDL.destroyWindow ctxWindow
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  Image.quit
  SDL.quit

withContext    :: Integral a => SDL.V2 a -> SDL.V2 a -> (Context -> IO a) -> IO a
withContext winPos winSize act = do
  context <- initializeContext winPos winSize
  result  <- act context
  freeContext    context
  return result