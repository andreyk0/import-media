{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module MediaInfo(
  MediaInfo(..)
, MediaType(..)
, parseMediaInfo
) where


import qualified Data.Conduit.Shell as Sh
import qualified Data.Conduit.List as CL
import           Data.Time
import           Data.Maybe
import qualified Data.Map as M
import           System.Directory
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Text.XML
import           Text.XML.Cursor
import qualified Text.XML.Stream.Parse as XMLParse



data MediaInfo =
  MediaInfo { mediaFilePath:: FilePath
            , mediaFormat:: Text
            , mediaDate:: UTCTime
            , mediaType:: MediaType
            }
  deriving (Eq, Show)

data MediaType =
  Image { imageCameraMake :: Maybe Text
        , imageCameraModel :: Maybe Text
        }
  | Video
  | Unknown
  deriving (Eq, Show)


-- | Parse output of mediainfo
parseMediaInfo :: FilePath -> IO MediaInfo
parseMediaInfo mediaFile = do

  mediaInfoXmlDoc <- Sh.run $ ( (Sh.mediainfo "--output=XML" mediaFile) Sh.$| parseMediaInfoXML )

  let rootDocCursor = fromDocument mediaInfoXmlDoc

      maybeTrackElement t a = listToMaybe $ rootDocCursor $/
                                element "File" &/
                                element "track" >=>
                                attributeIs "type" t &/ a

      maybeGeneralFormat = maybeTrackElement "General" $ element "Format" &/ content
      maybeEncodedDate = parseEncodedDate =<< (maybeTrackElement "General" $ element "Encoded_date" &/ content)

      maybeVideoFormat = maybeTrackElement "Video" $ element "Format" &/ content -- defined when video
      maybeImageFormat = maybeTrackElement "Image" $ element "Format" &/ content -- defined when image

      generalFormat = fromMaybe "UNKNOWN" $ listToMaybe $ catMaybes [maybeGeneralFormat, maybeVideoFormat, maybeImageFormat] -- prefer general format, more aligned with file extensions

  fileModUTCTime <- getModificationTime mediaFile

  let unknownMediaInfo = MediaInfo mediaFile generalFormat fileModUTCTime Unknown

  if (isJust maybeVideoFormat)
  then
    let vidEncDate = fromMaybe fileModUTCTime maybeEncodedDate
    in return $ unknownMediaInfo { mediaType = Video, mediaDate = vidEncDate }

  else
   case maybeImageFormat
     of Just "JPEG" -> parseJHead unknownMediaInfo -- mediainfo doesn't parse all JPG headers, use jhead for that
        Just _      -> return $ unknownMediaInfo { mediaType = Image Nothing Nothing }
        Nothing     -> return $ unknownMediaInfo


-- <Encoded_date>UTC 2015-04-11 14:26:03</Encoded_date> -- from GoPro
parseEncodedDate :: Text -> Maybe UTCTime
parseEncodedDate t = parseTimeM True defaultTimeLocale "%Z %Y-%m-%d %H:%M:%S" (T.unpack t)


-- | Specialized conduit to parse XML output into a Document
parseMediaInfoXML :: Sh.Segment Document
parseMediaInfoXML = Sh.conduit (XMLParse.parseBytesPos def Sh.$= fromEvents)


-- | Parse jhead output, fill in more mediainfo fields
parseJHead :: MediaInfo
           -> IO MediaInfo
parseJHead unknownMediaInfo = do
  jheadLines <- getJHeadOutputLines (mediaFilePath unknownMediaInfo)
  let kvPairs = M.fromList $ map (\(k,v) -> (T.strip k, T.strip (T.drop 1 v))) $ map (T.break (== ':')) jheadLines
  return $ unknownMediaInfo { mediaDate = fromMaybe (mediaDate unknownMediaInfo) (parseJHeadDate =<< M.lookup "Date/Time" kvPairs)
                            , mediaType = Image (M.lookup "Camera make" kvPairs) (M.lookup "Camera model" kvPairs)
                            }


-- | Get lines of
getJHeadOutputLines :: FilePath -> IO [Text]
getJHeadOutputLines fp = do
  let cLines = Sh.conduit (CL.map (T.lines . decodeUtf8) Sh.$= CL.consume)
  tls <- Sh.run $ (Sh.jhead "-se" fp Sh.$| cLines)
  return $ concat tls

-- Date/Time    : 2015:03:31 13:04:01 -- from jhead
parseJHeadDate :: Text -> Maybe UTCTime
parseJHeadDate t = parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S" (T.unpack t)
