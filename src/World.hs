{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module World where

import           Lens.Micro.TH
import           Control.Restartable.Checkpoint
import           Control.Restartable.Initial
import           Data.Aeson
import           GHC.Generics
import           Linear.V2
import           Data.Int(Int32)

data World = World { _gameTime  :: Int
                   , _windowPos :: V2 Int32 }
  deriving (Eq, Show, Generic)

makeLenses ''World

instance FromJSON a => FromJSON (V2 a)
instance ToJSON   a => ToJSON   (V2 a)
instance Initial  a => Initial  (V2 a)

instance ToJSON World
instance FromJSON World where
  parseJSON = initially $
                World 0 (V2 0 0)
instance Initial World
