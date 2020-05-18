{-# LANGUAGE OverloadedStrings #-}

module Tautulli.Request () where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req

apiKey :: String
apiKey = "ef77bd7822ba4ffcad33970338ac53b0"

getTautulliResponse :: (QueryParam a) => String -> [a] -> IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
getTautulliResponse cmd queryParams = runReq defaultHttpConfig $ do
  r <-
    req
      POST -- method
      (https "media.hpg.nz" /: "tautilli" /: "api" /: "v2") -- safe by construction URL
      NoReqBody -- use built-in options or add your own
      jsonResponse $ -- specify how to interpret response
        "apikey" =: apiKey <>
        "cmd" =: cmd <>
        basicAuth "" ""
  liftIO $ print (responseBody r :: Value)

getTautulliResponseNoParams :: String -> IO ()
getTautulliResponseNoParams cmd = getTautulliResponse cmd ([] :: [FormUrlEncodedParam])

getArnold = getTautulliResponseNoParams "arnold"