module Lib
    ( someFunc
    ) where

import Tautulli.Request

someFunc :: IO ()
someFunc = getLibraryPage 1
