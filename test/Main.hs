{-# language OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import Text.Diagnostic
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "unit tests" $ do
      it "1" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents" $
            caret 0 0 (Message "this is a message")
          output =
            Lazy.unlines
            [ "filename:0:0: error: this is a message"
            , "  |"
            , "0 | here are the file contents"
            , "  | ^"
            ]
        input `shouldBe` output
