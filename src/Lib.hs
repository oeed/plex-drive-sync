module Lib (
  readConfig
) where

import Tautulli.Request
import Tautulli.Library
import Config (readConfig)

someFunc :: IO ()
someFunc = getLibraryPage 1
