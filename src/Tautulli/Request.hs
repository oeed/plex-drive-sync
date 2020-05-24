{-# LANGUAGE OverloadedStrings #-}

module Tautulli.Request (
  getTautulliResponse,
  getTautulliResponseNoParams
) where

import Data.Text
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key)
import Network.Wreq
import Control.Lens
import Control.Exception
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy
import Config (APIConfig, api, key, username, password, url, readConfig)
import Data.Maybe (fromJust)

data TautulliException = EmptyResponse | FailureResponse deriving (Show)

instance Exception TautulliException

getTautulliResponse :: APIConfig -> Text -> (Options -> Options) -> IO Value
getTautulliResponse config cmd options = do
  let opts = defaults & param "apikey" .~ [Config.key config]
                      & param "cmd" .~ [cmd]
                      & options
                      & auth ?~ basicAuth (encodeUtf8 (username config)) (encodeUtf8 (password config))
  r <- getWith opts (url config)
  case r ^? responseBody . Data.Aeson.Lens.key "response" . Data.Aeson.Lens.key "data" of
    Nothing -> throw EmptyResponse
    Just body -> return body

getTautulliResponseNoParams config cmd = getTautulliResponse config cmd id

getArnold config = getTautulliResponseNoParams config "arnold"