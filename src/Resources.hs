{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Load static assets and free them after the game ends.
--
-- TODO: use generics to make a managed resource class with `barbies`?
module Resources(Resources(..), loadResources, freeResources) where

import RIO
import qualified SDL.Font                 as Font
import qualified SDL.Image                as Image

import Paths_alien_expedition(getDataFileName)
data Resources = Resources {
    rFont :: Font.Font
  }

loadResources renderer = do
  Font.initialize
  Image.initialize [Image.InitPNG]
  Resources <$> Font.load "assets/Loja.otf" 72

freeResources Resources {..} = do
  Font.free rFont
  Font.quit
  Image.quit
