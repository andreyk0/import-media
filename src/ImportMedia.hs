{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module ImportMedia (
  IMConfig(..)
, importMediaFile
) where

import           Control.Conditional
import qualified Data.Char           as C
import qualified Data.Conduit.Shell  as Sh
import           Data.List
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time.Clock
import           Data.Time.Format
import           MediaInfo
import           System.Directory
import           System.FilePath
import           System.IO


data IMConfig =
  IMConfig { verbose        :: Bool
           , noop           :: Bool
           , ignoreFileName :: Bool
           , mediaRoot      :: FilePath
           , mediaFilePaths :: [FilePath]
           } deriving (Show)


importMediaFile:: IMConfig -> MediaInfo -> IO ()
importMediaFile imc@IMConfig{..} mInf@MediaInfo{..} =
  case mediaType
    of Unknown -> hPutStrLn stderr $ "SKIPPING " <> mediaFilePath <> ", un-supported media format " <> T.unpack mediaFormat
       _ -> do let tfp = targetFilePath imc mInf
                   tfn = targetFileName imc mInf
                   tf  = tfp </> tfn
                   sf  = mediaFilePath

               ifM (doesFileExist tf)
                   (hPutStrLn stderr $ "SKIPPING [" <> tf <> "], target file already exists!")
                   (do when (verbose || noop)
                            (putStrLn $ sf <> " -> " <> tf)

                       unless noop $ Sh.run $ do Sh.mkdir (addV "-p") tfp
                                                 Sh.cp "-a" sf tf
                                                 Sh.chmod "644" tf
                   )

  where addV s = if verbose then s <> "v" else s


-- | Full path to target directory
targetFilePath:: IMConfig -> MediaInfo -> FilePath
targetFilePath IMConfig{..} MediaInfo{..} =
  let rootSubDir = case mediaType
                     of Unknown   -> "unknown"
                        Video     -> "videos"
                        Image _ _ -> "photos"
   in mediaRoot </> rootSubDir </> utcTimeToFilePathPart mediaDate


-- | File name part of the target
targetFileName:: IMConfig -> MediaInfo -> FilePath
targetFileName IMConfig{..} MediaInfo{..} =
  let (_, fnPlusExt) = splitFileName mediaFilePath
      (fn, ext) = splitExtension $ normalizeFn fnPlusExt
      fnPrefix = utcTimeToFileNamePart mediaDate
      lowerFn = if ignoreFileName then "" else fn
      fnParts = case mediaType
                  of Unknown -> [fnPrefix, lowerFn]
                     Video -> [fnPrefix, lowerFn]
                     Image _ cModel -> [fnPrefix, maybeTextToFileNamePart cModel, lowerFn]
   in intercalate "-" (filter (not . null) fnParts) <> ext


utcTimeToFileNamePart:: UTCTime -> String
utcTimeToFileNamePart = formatTime defaultTimeLocale "%Y-%m-%d-%H%M%S"

utcTimeToFilePathPart:: UTCTime -> String
utcTimeToFilePathPart = formatTime defaultTimeLocale "%Y/%m/%d"

maybeTextToFileNamePart:: Maybe Text -> String
maybeTextToFileNamePart (Just t) = normalizeFn (filter C.isAlphaNum (T.unpack t))
maybeTextToFileNamePart Nothing = ""

normalizeFn:: String -> String
normalizeFn = map (underscoreToSlash . C.toLower)
  where underscoreToSlash c | c == '_'  = '-'
                            | otherwise =  c
