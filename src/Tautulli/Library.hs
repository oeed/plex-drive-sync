module Tautulli.Library (
  getLibraryMediaInfoPage
) where

import Tautulli.Request
import Config (APIConfig)

-- getLibraryMediaInfoPage :: APIConfig -> Int :: IO ()
getLibraryMediaInfoPage config sectionID = getTautulliResponse config "get_library_media_info" (param "section_id" .~ [show sectionID])
