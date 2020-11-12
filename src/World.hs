{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module World where

import           Lens.Micro.TH
import           Control.Restartable.Checkpoint
import           Control.Restartable.Initial
import           Data.Aeson
import           GHC.Generics
import           Linear.V2
--import           Data.Int (Int32)

data World = World { _gameTime   ::    Int
                   , _windowPos  :: V2 Int
                   , _windowSize :: V2 Int }
  deriving (Eq, Show, Generic)

makeLenses ''World

instance (FromJSON a
         ,Num      a) => FromJSON (V2 a) where
  parseJSON = initially $ V2 0 0
instance ToJSON   a => ToJSON   (V2 a)
instance (Num a,Initial a) => Initial  (V2 a)

instance Initial Int

instance ToJSON   World
instance FromJSON World where
  parseJSON = initially $
                World 0 initial initial
instance Initial  World
