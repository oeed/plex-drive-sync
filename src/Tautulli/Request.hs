{-# LANGUAGE OverloadedStrings #-}

module Tautulli.Request (
  getTautulliResponse,
  getTautulliResponseNoParams
) where

-- import Control.Monad.IO.Class
-- import Data.Aeson
import Data.Text
import Data.Aeson.Lens (_String, key)
import Network.Wreq
import Control.Lens
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy
import Config (APIConfig, api, key, username, password, url, readConfig)

getTautulliResponse :: APIConfig -> Text -> (Options -> Options) -> IO ByteString
getTautulliResponse config cmd options = do
  let opts = defaults & param "apikey" .~ [Config.key config]
                      & param "cmd" .~ [cmd]
                      & options
                      & auth ?~ basicAuth (encodeUtf8 (username config)) (encodeUtf8 (password config))
  r <- getWith opts (url config)
  return (r ^. responseBody)

getTautulliResponseNoParams config cmd = getTautulliResponse config cmd id

getArnold config = getTautulliResponseNoParams config "arnold"