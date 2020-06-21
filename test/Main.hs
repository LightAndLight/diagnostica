{-# language OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy as Lazy
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
            emit 0 (Caret 0) (Message "this is a message")
          output =
            Lazy.unlines
            [ "filename:0:0: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m0 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m\ESC[91;1m^\ESC[39;0m"
            ]
        input `shouldBe` output
      it "2" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents" $
            emit 0 (Span 5 8) (Message "this is a message")
          output =
            Lazy.unlines
            [ "filename:0:5: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m0 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m     \ESC[91;1m^^^\ESC[39;0m"
            ]
        input `shouldBe` output
      it "3" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents" $
            emit 0 (Caret 0) (Message "this is a message") <>
            emit 0 (Span 5 8) (Message "this is another message")
          output =
            Lazy.unlines
            [ "filename:0:0: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m0 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m\ESC[91;1m^\ESC[39;0m"
            , ""
            , "filename:0:5: \ESC[91;1merror: \ESC[39;0mthis is another message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m0 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m     \ESC[91;1m^^^\ESC[39;0m"
            ]
        input `shouldBe` output
