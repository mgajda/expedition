{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           ClassyPrelude
import           Control.Restartable.Checkpoint
import           Control.Restartable.Initial
import           Data.Aeson
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Foreign.C.Types(CInt)
import           Lens.Micro
import           Lens.Micro.Extras
import           Lens.Micro.TH
import           Resources

import qualified SDL
import qualified SDL.Event                as SDL
import qualified SDL.Font                 as Font
import qualified SDL.Image                as Image
import qualified SDL.Input.Keyboard.Codes as SDL
import qualified SDL.Mixer                as Mixer
import qualified SDL.Vect                 as SDL

import           SDLContext
import           System.Random
import           World

data Config = Config {
    cContext   :: Context
  , cResources :: Resources
  }

-- | Accessors
cRenderer  = ctxRenderer . cContext
cWindow    = ctxWindow   . cContext

{-
-- | Rapid entry
develMain :: IO ()
develMain = do
  rapid 0 $ \rapidRef -> do
    cContext       <- createRef rapidRef "SDL context" initializeContext
    restart rapidRef "game" $ do
      cResources     <- loadResources cRenderer
      let config      = Config {..}
      restartable "game.save" $ runRIO config run
 -}

-- | Normal entry
main :: IO ()
main = do
  cContext   <- initializeContext
  cResources <- loadResources $ ctxRenderer cContext
  let config  = Config {..}
  restartable "game.save" $ run config 0
  freeResources cResources
  freeContext   cContext

keycodeToAction SDL.KeycodeSpace = step
keycodeToAction SDL.Keycode2     = gameTime `over` (*2)
keycodeToAction other            = id

step = gameTime `over` (+1)

data Texture = Texture SDL.Texture (SDL.V2 CInt)

renderTexture r (Texture t size) xy = do
  SDL.copy r t Nothing (Just $ SDL.Rectangle xy size)

renderModel :: Config -> CInt -> CInt -> World -> IO ()
renderModel config w h g = do
    SDL.clear $ cRenderer config
    textSurface <- Font.solid (rFont $ cResources config) white $ T.pack $ show $ view gameTime g
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

run, nextEvent :: Config
               -> SDL.Timestamp -- last keyboard event time (for debouncing)
               -> World
               -> IO (World, Ending)
run config ts g = do
  SDL.V2 w h <- SDL.get $ SDL.windowSize $ cWindow config
  renderModel config w h g
  nextEvent   config ts g

debouncePeriod = 200

nextEvent config ts g = do
  evt <- SDL.waitEvent
  SDL.V2 w h <- SDL.get $ SDL.windowSize $ cWindow config
  case SDL.eventPayload evt of
    SDL.QuitEvent ->
      return (g, Quit)
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = SDL.KeycodeQ } }) ->
      return (g, Quit)
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = SDL.KeycodeR } }) ->
      return (g, Restart)
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = kcode        } }) -- debounce
      | ts+debouncePeriod > SDL.eventTimestamp evt -> run config ts g
    SDL.KeyboardEvent (SDL.KeyboardEventData { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = kcode        }
                                             , SDL.keyboardEventKeyMotion = SDL.Pressed }) -> do    
      run config (SDL.eventTimestamp evt) (keycodeToAction kcode g)
    other         ->
      run config ts g
