{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}


module Main where

import           Control.Conditional
import           Control.Monad       hiding (unless, when)
import           ImportMedia
import           MediaInfo
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.IO

cmdLineArgs :: Maybe FilePath -- ^ default media root, if any, from an env var
            -> Parser IMConfig
cmdLineArgs maybeMr =
  IMConfig
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
        <> maybe mempty value maybeMr
        <> showDefault
        <> help "media root directory, defaults to MEDIA_ROOT env var" )

     <*> some (argument str (metavar "MEDIA_FILES..."))


main :: IO ()
main = do
  maybeMRoot <- lookupEnv "MEDIA_ROOT"

  args@IMConfig{..} <-
    execParser $ info (helper <*> cmdLineArgs maybeMRoot)
                      ( fullDesc
                     <> header "Imports media files (photo/video) to a media root, organized by date." )

  ifM (doesDirectoryExist mediaRoot)
      (return ())
      (error $ "Media root [" <> mediaRoot <> "] does not exist!")

  forM_ mediaFilePaths $ \fp ->
    ifM (doesFileExist fp)
        (do x <- parseMediaInfo fp
            importMediaFile args x)
        (hPutStrLn stderr $ "Skipping " <> fp <> ", it is not a regular file ...")
