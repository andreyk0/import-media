{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module MediaInfo(
  MediaInfo(..)
, MediaType(..)
, parseMediaInfo
) where


import qualified Data.Conduit.List     as CL
import           Data.Conduit.Shell    (($|), (.|))
import qualified Data.Conduit.Shell    as Sh
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           Data.Time
import           System.Directory
import           Text.XML
import           Text.XML.Cursor       hiding (($|))
import qualified Text.XML.Stream.Parse as XMLParse


default (Text)


data MediaInfo =
  MediaInfo { mediaFilePath :: FilePath
            , mediaFormat   :: Text
            , mediaDate     :: UTCTime
            , mediaType     :: MediaType
            }
  deriving (Eq, Show)

data MediaType =
  Image { imageCameraMake  :: Maybe Text
        , imageCameraModel :: Maybe Text
        }
  | Video
  | Unknown
  deriving (Eq, Show)


-- | Parse output of mediainfo
parseMediaInfo :: FilePath -> IO MediaInfo
parseMediaInfo mediaFile = do

  mediaInfoXmlDoc <- Sh.run (Sh.mediainfo "--output=XML" mediaFile $| parseMediaInfoXML)

  let rootDocCursor = fromDocument mediaInfoXmlDoc

      maybeTrackElement t a = listToMaybe $ rootDocCursor $/
                                laxElement "media" &/
                                laxElement "track" >=>
                                attributeIs "type" t &/ a

      maybeGeneralFormat = maybeTrackElement "General" $ laxElement "Format" &/ content
      maybeEncodedDate = parseEncodedDate =<< maybeTrackElement "General" (laxElement "Encoded_date" &/ content)

      maybeVideoFormat = maybeTrackElement "Video" $ laxElement "Format" &/ content -- defined when video
      maybeImageFormat = maybeTrackElement "Image" $ laxElement "Format" &/ content -- defined when image

      generalFormat = fromMaybe "UNKNOWN" $ listToMaybe $ catMaybes [maybeGeneralFormat, maybeVideoFormat, maybeImageFormat] -- prefer general format, more aligned with file extensions

  fileModUTCTime <- getModificationTime mediaFile

  let unknownMediaInfo = MediaInfo mediaFile generalFormat fileModUTCTime Unknown

  if isJust maybeVideoFormat
  then
    let vidEncDate = fromMaybe fileModUTCTime maybeEncodedDate
    in return $ unknownMediaInfo { mediaType = Video, mediaDate = vidEncDate }

  else
   case maybeImageFormat
     of Just "JPEG" -> parseJHead unknownMediaInfo -- mediainfo doesn't parse all JPG headers, use jhead for that
        Just _      -> return $ unknownMediaInfo { mediaType = Image Nothing Nothing }
        Nothing     -> return unknownMediaInfo


-- <Encoded_date>UTC 2015-04-11 14:26:03</Encoded_date> -- from GoPro
parseEncodedDate :: Text -> Maybe UTCTime
parseEncodedDate t = parseTimeM True defaultTimeLocale "%Z %Y-%m-%d %H:%M:%S" (T.unpack t)


-- | Specialized conduit to parse XML output into a Document
parseMediaInfoXML :: Sh.Segment Document
parseMediaInfoXML = Sh.conduit (XMLParse.parseBytesPos def .| fromEvents)


-- | Parse jhead output, fill in more mediainfo fields
parseJHead :: MediaInfo
           -> IO MediaInfo
parseJHead unknownMediaInfo = do
  jheadLines <- getJHeadOutputLines (mediaFilePath unknownMediaInfo)
  let kvPairs = M.fromList $ (\(k,v) -> (T.strip k, T.strip (T.drop 1 v))) . T.break (== ':') <$> jheadLines
  return $ unknownMediaInfo { mediaDate = fromMaybe (mediaDate unknownMediaInfo) (parseJHeadDate =<< M.lookup "Date/Time" kvPairs)
                            , mediaType = Image (M.lookup "Camera make" kvPairs) (M.lookup "Camera model" kvPairs)
                            }


-- | Get lines of
getJHeadOutputLines :: FilePath -> IO [Text]
getJHeadOutputLines fp = do
  let cLines = Sh.conduit (CL.map (T.lines . decodeUtf8) .| CL.consume)
  tls <- Sh.run (Sh.jhead "-se" fp $| cLines)
  return $ concat tls

-- Date/Time    : 2015:03:31 13:04:01 -- from jhead
parseJHeadDate :: Text -> Maybe UTCTime
parseJHeadDate t = parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S" (T.unpack t)
