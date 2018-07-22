{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module MediaInfoSpec where

import           MediaInfo
import           Test.Hspec

mediaInfoSpec :: Spec
mediaInfoSpec = describe "MediaInfo" $ do
  MediaInfo{..} <- runIO $ parseMediaInfo "test-resources/eg.jpg"
  it "parse jpg / file path" $ mediaFilePath `shouldBe` "test-resources/eg.jpg"
  it "parse jpg / format" $ mediaFormat `shouldBe` "JPEG"
  it "parse jpg / date" $ show mediaDate `shouldBe` "2018-07-01 21:49:47 UTC"
  it "parse jpg / type" $ mediaType `shouldBe` Image (Just "NIKON CORPORATION") (Just "NIKON Df")
