{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}


module Main where

import Control.Conditional
import Control.Monad hiding(unless,when)
import Data.List
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format
import MediaInfo
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import qualified Data.Char as C
import qualified Data.Conduit.Shell as Sh
import           Data.Text(Text)
import qualified Data.Text as T


data CmdLineArgs =
  CmdLineArgs { verbose:: Bool
              , noop:: Bool
              , ignoreFileName:: Bool
              , mediaRoot:: FilePath
              , mediaFilePaths:: [FilePath]
              } deriving (Show)


cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs =
  CmdLineArgs
     <$> switch
         ( long "verbose"
        <> short 'v'
        <> help "be verbose" )

     <*> switch
         ( long "noop"
        <> short 'n'
        <> help "do not import files, show what would be done" )

     <*> switch
         ( long "ignore-file-name"
        <> short 'i'
        <> help "don't include original file name in the target file" )

     <*> option str
         ( long "media-root"
        <> short 'm'
        <> help "media root directory" )

     <*> some (argument str (metavar "MEDIA_FILES..."))


cmdLineParser :: ParserInfo CmdLineArgs
cmdLineParser =
  info (helper <*> cmdLineArgs)
       ( fullDesc
      <> header "Imports media files (photo/video) to a media root, organized by date." )


main :: IO ()
main = do
  args <- execParser cmdLineParser

  ifM (doesDirectoryExist (mediaRoot args))
      (return ())
      (error $ "Media root [" <> mediaRoot args <> "] does not exist!")

  forM_ (mediaFilePaths args) $ \fp ->
    ifM (doesFileExist fp)
        (do x <- parseMediaInfo fp
            importMediaFile args x)
        (hPutStrLn stderr $ "Skipping " <> fp <> ", it is not a regular file ...")


importMediaFile:: CmdLineArgs -> MediaInfo -> IO ()
importMediaFile args mInf =
  case (mediaType mInf)
    of Unknown -> hPutStrLn stderr $ "SKIPPING " <> mediaFilePath mInf <> ", un-supported media format " <> (T.unpack . mediaFormat) mInf
       _ -> do let tfp = targetFilePath args mInf
                   tfn = targetFileName args mInf
                   tf  = tfp </> tfn
                   sf  = mediaFilePath mInf

               ifM (doesFileExist tf)
                   (hPutStrLn stderr $ "SKIPPING [" <> tf <> "], target file already exists!")
                   (do when ( (verbose args) || (noop args) )
                            (putStrLn $ sf <> " -> " <> tf)

                       unless (noop args) $ do (Sh.run (do Sh.mkdir (addV "-p") tfp
                                                           Sh.cp "-a" sf tf
                                                           Sh.chmod "644" tf))
                   )

  where addV s = if (verbose args) then s <> "v" else s


-- | Full path to target directory
targetFilePath:: CmdLineArgs -> MediaInfo -> FilePath
targetFilePath args mInf =
  let rootSubDir = case (mediaType mInf)
                     of Unknown   -> "unknown"
                        Video     -> "videos"
                        Image _ _ -> "photos"
   in (mediaRoot args) </> rootSubDir </> (utcTimeToFilePathPart . mediaDate) mInf


-- | File name part of the target
targetFileName:: CmdLineArgs -> MediaInfo -> FilePath
targetFileName args mInf =
  let (_, fnPlusExt) = splitFileName (mediaFilePath mInf)
      (fn, ext) = splitExtension $ normalizeFn fnPlusExt
      fnPrefix = utcTimeToFileNamePart (mediaDate mInf)
      lowerFn = if (ignoreFileName args) then "" else fn
      fnParts = case (mediaType mInf)
                  of Unknown -> [fnPrefix, lowerFn]
                     Video -> [fnPrefix, lowerFn]
                     Image _ cModel -> [fnPrefix, maybeTextToFileNamePart cModel, lowerFn]
   in (intercalate "-" $ filter (not . null) fnParts) <> ext


utcTimeToFileNamePart:: UTCTime -> String
utcTimeToFileNamePart = formatTime defaultTimeLocale "%Y-%m-%d-%H%M%S"

utcTimeToFilePathPart:: UTCTime -> String
utcTimeToFilePathPart = formatTime defaultTimeLocale "%Y/%m/%d"

maybeTextToFileNamePart:: Maybe Text -> String
maybeTextToFileNamePart (Just t) = normalizeFn (filter (C.isAlphaNum) (T.unpack t))
maybeTextToFileNamePart Nothing = ""

normalizeFn:: String -> String
normalizeFn = map (underscoreToSlash . C.toLower)
  where underscoreToSlash c | c == '_'  = '-'
                            | otherwise =  c
