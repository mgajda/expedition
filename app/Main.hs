{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Font
import qualified SDL.Event as SDL
import qualified SDL.Input.Keyboard.Codes as SDL
import qualified Data.Text.IO as T
import System.Random
import Control.Restartable.Checkpoint
--import Optics.Optic
import Optics
import Optics.TH
--import KeyState

width = 640
height = 480

data Config = Config {
    cWindow    :: SDL.Window
  , cRenderer  :: SDL.Renderer
  , cResources :: Resources
  }

data Resources = Resources {
  }

data Game = Game { _gameTime :: Int }

makeLenses ''Game

loadResources renderer     = return Resources
freeResources Resources {} = return ()

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Font.initialize
  Mixer.openAudio Mixer.defaultAudio 256
  cWindow    <- SDL.createWindow "AlienExp" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1280 1024 }
  cRenderer  <- SDL.createRenderer cWindow (-1) SDL.defaultRenderer
  cResources <- loadResources cRenderer
  let config = Config {..}
  let state = Game 0
  run config state
  --restartable $ run config
  SDL.destroyWindow cWindow
  freeResources     cResources
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  SDL.quit

keycodeToAction SDL.KeycodeSpace = step
keycodeToAction other            = id

step = gameTime `over` (+1)

run config g = do
  evt <- SDL.eventPayload <$> SDL.waitEvent
  case evt of
    SDL.QuitEvent -> return ()
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = kcode } }) ->
      run config $ keycodeToAction kcode g
    other     -> run config g
