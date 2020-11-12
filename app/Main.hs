{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import qualified SDL
import qualified SDL.Mixer                as Mixer
import qualified SDL.Font                 as Font
import qualified SDL.Event                as SDL
import qualified SDL.Image                as Image
import qualified SDL.Input.Keyboard.Codes as SDL
import qualified SDL.Vect                 as SDL
import qualified Data.Text.IO             as T
import           RIO
import           System.Random
import           Lens.Micro.TH
import           Foreign.C.Types(CInt)
import           Control.Restartable.Checkpoint
import           Control.Restartable.Initial
import           Data.Aeson

import Resources
import World

data Config = Config {
    cWindow    :: SDL.Window
  , cRenderer  :: SDL.Renderer
  , cResources :: Resources
  }

screenWidth  = 1280
screenHeight = 1024

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]

  Mixer.openAudio Mixer.defaultAudio 256
  cWindow    <- SDL.createWindow "AlienExp" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 screenWidth screenHeight }
  cRenderer  <- SDL.createRenderer cWindow (-1) SDL.defaultRenderer
  cResources <- loadResources cRenderer
  let config = Config {..}
  restartable "game.save" $ run config
  SDL.destroyWindow cWindow
  freeResources     cResources
  Mixer.closeAudio
  Mixer.quit
  Font.quit
  Image.quit
  SDL.quit

keycodeToAction SDL.KeycodeSpace = step
keycodeToAction other            = id

step = gameTime `over` (+1)

data Texture = Texture SDL.Texture (SDL.V2 CInt)

--renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> IO ()
renderTexture r (Texture t size) xy = do
  SDL.copy r t Nothing (Just $ SDL.Rectangle xy size)

renderModel :: Config -> World -> IO ()
renderModel config g = do
    SDL.clear $ cRenderer config
    textSurface <- Font.solid (rFont $ cResources config) white $ RIO.tshow $ view gameTime g
    texture <- SDL.createTextureFromSurface (cRenderer config) textSurface
    SDL.freeSurface textSurface
    --winSurface  <- SDL.getWindowSurface $ cWindow config
    SDL.copy (cRenderer config) texture Nothing Nothing
    SDL.present $ cRenderer config
    --SDL.surfaceBlit textSurface Nothing winSurface Nothing
    --renderTexture textSurface $ SDL.V2 0 0
    return ()
  where
    white = SDL.V4 0xff 0xff 0xff 0x00

run, nextEvent :: Config -> World -> IO (World, Ending)
run config g = do
  renderModel config g
  nextEvent   config g

nextEvent config g = do
  evt <- SDL.eventPayload <$> SDL.waitEvent
  case evt of
    SDL.QuitEvent ->
      return (g, Quit)
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = SDL.KeycodeEscape } }) ->
      return (g, Quit)
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = SDL.KeycodeR } }) ->
      return (g, Restart)
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = kcode } }) ->
      run config $ keycodeToAction kcode g
    other         ->
      run config g
