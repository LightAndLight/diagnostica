{-# language OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Text.Diagnostic (Message(..), render, caret, span)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "unit tests" $ do
      it "1" $ do
        let
          input =
            render "filename" "here are the file contents" $
            caret 0 0 (Message "this is a message")
          output =
            Lazy.unlines
            [ "filename:0:0: error: this is a message"
            , "  |"
            , "0 | here are the file contents"
            , "  | ^"
            ]
        input `shouldBe` output
