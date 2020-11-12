{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module World where

import           Lens.Micro.TH
import           Control.Restartable.Checkpoint
import           Control.Restartable.Initial
import           Data.Aeson
import           GHC.Generics

data World = World { _gameTime :: Int }
  deriving (Eq, Show, Generic)

makeLenses ''World

instance ToJSON World
instance FromJSON World where
  parseJSON = initially $ World 0
instance Initial World
