{-# LANGUAGE DeriveGeneric #-} -- (2)

module Config (readConfig, Config, APIConfig, api, username, password, key, url, pageSize) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Text
import GHC.Generics
import Data.Aeson

newtype Config = Config { api :: APIConfig } deriving (Show, Generic)
instance FromJSON Config 

data APIConfig = APIConfig {
  username :: Text,
  password :: Text,
  key :: Text,
  url :: String,
  pageSize :: Int
} deriving (Show, Generic)
instance FromJSON APIConfig 

readConfig :: IO Config
readConfig = do
    content <- BS.readFile "config.yaml"
    let parsedContent = Y.decodeThrow content :: Maybe Config
    case parsedContent of
        Nothing -> error "Could not parse config file."
        (Just config) -> return config

