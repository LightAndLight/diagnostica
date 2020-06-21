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
    describe "examples" $ do
      it "1" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents" $
            caret 0 0 (Message "this is a message")
          output =
            Lazy.unlines
            [ "filename:0:0: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m0 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m\ESC[91;1m^\ESC[39;0m"
            ]
        input `shouldBe` output
