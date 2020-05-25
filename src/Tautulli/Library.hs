{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tautulli.Library (
  getMediaItems
) where


import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Tautulli.Request
import Network.Wreq
import Control.Lens
import Data.Aeson (Value, FromJSON)
import Data.Aeson.Types
import Config
import Data.Aeson.Lens (_String, key)
import GHC.Generics
import qualified Data.Vector as V
import Data.Maybe (fromJust)

intToText :: Integral a => a -> T.Text
intToText = T.toStrict . B.toLazyText . B.decimal

data MediaItem = MediaItem {
  title :: T.Text,
  mediaType :: T.Text,
  addedAt :: Int,
  last_played :: Maybe Int,
  file_size :: Int
} deriving (Generic, Show)

instance FromJSON MediaItem where
  parseJSON = withObject "MediaItem" $ \o -> do
    title <- o .: "title"
    mediaType <- o .: "media_type"
    -- this v v v
    x <- o .: "added_at" :: Parser String
    let addedAt = read x
    -- 
    last_played <- o .: "last_played"
    file_size <- o .: "file_size"
    return MediaItem{..}

-- instance FromJSON MediaItem

parsePage :: Value -> Parser [MediaItem]
parsePage = withObject "response" $ \o -> do
  content <- o .: "data"
  parseMediaItem content

parseMediaItem :: Value -> Parser [MediaItem]
parseMediaItem (Array value) = mapM parseJSON (V.toList value)
parseMediaItem _ = fail "expected an array"

getLibraryMediaInfo config sectionID = getTautulliResponse config "get_library_media_info" ((param "section_id" .~ [intToText sectionID]) . (param "length" .~ [intToText (pageSize config)]))

getMediaItems :: Integral a => a -> IO [MediaItem]
getMediaItems sectionID = do
  config <- readConfig
  r <- getLibraryMediaInfo (api config) sectionID
  case parse parsePage r of
    Success items -> return items
    Error a -> fail a