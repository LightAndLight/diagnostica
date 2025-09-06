{-# language OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy as Lazy
import Text.Diagnostic
import Test.Hspec
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.ByteString as ByteString

main :: IO ()
main =
  hspec $ do
    describe "examples - 0-indexed" $ do
      it "1" $ do
        let
          input =
            render (defaultConfig { zeroIndexed = True }) "filename" "here are the file contents" $
            emit (Pos 0 0) Caret "this is a message"
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
            render (defaultConfig { zeroIndexed = True }) "filename" "here are the file contents" $
            emit (Pos 0 5) (Span 3) "this is a message"
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
            render (defaultConfig { zeroIndexed = True }) "filename" "here are the file contents" $
            emit (Pos 0 0) Caret "this is a message" <>
            emit (Pos 0 5) (Span 3) "this is another message"
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
      it "4" $ do
        let
          input =
            render (defaultConfig { zeroIndexed = True }) "filename" "here are the file contents\nthis is the second line" $
            emit (Pos 0 0) Caret "this is a message" <>
            emit (Pos 0 5) (Span 3) "this is another message" <>
            emit (Offset 39) Caret "this is another another message"
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
            , ""
            , "filename:1:12: \ESC[91;1merror: \ESC[39;0mthis is another another message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m1 | \ESC[39;0mthis is the second line"
            , "\ESC[34;1m  | \ESC[39;0m            \ESC[91;1m^\ESC[39;0m"
            ]
        input `shouldBe` output
      it "non-ascii" $ do
        let
          line1 = "😎😎😎😎😎😎"
          line2 = "error is here"
          input =
            render (defaultConfig { zeroIndexed = True }) "filename" (line1 <> "\n" <> line2) $
            emit
              (Offset $ ByteString.length (Text.Encoding.encodeUtf8 line1) + ByteString.length "error is " + 1)
              (Span 4)
              "this is a message"
          output =
            Lazy.unlines
            [ "filename:0:0: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m0 | \ESC[39;0merror is here"
            , "\ESC[34;1m  |          \ESC[39;0m\ESC[91;1m^^^^\ESC[39;0m"
            ]
        input `shouldBe` output
    describe "examples - 1-indexed" $ do
      it "1" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents" $
            emit (Pos 1 1) Caret "this is a message"
          output =
            Lazy.unlines
            [ "filename:1:1: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m1 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m\ESC[91;1m^\ESC[39;0m"
            ]
        input `shouldBe` output
      it "2" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents" $
            emit (Pos 1 6) (Span 3) "this is a message"
          output =
            Lazy.unlines
            [ "filename:1:6: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m1 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m     \ESC[91;1m^^^\ESC[39;0m"
            ]
        input `shouldBe` output
      it "3" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents" $
            emit (Pos 1 1) Caret "this is a message" <>
            emit (Pos 1 6) (Span 3) "this is another message"
          output =
            Lazy.unlines
            [ "filename:1:1: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m1 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m\ESC[91;1m^\ESC[39;0m"
            , ""
            , "filename:1:6: \ESC[91;1merror: \ESC[39;0mthis is another message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m1 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m     \ESC[91;1m^^^\ESC[39;0m"
            ]
        input `shouldBe` output
      it "4" $ do
        let
          input =
            render defaultConfig "filename" "here are the file contents\nthis is the second line" $
            emit (Offset 0) Caret "this is a message" <>
            emit (Offset 5) (Span 3) "this is another message" <>
            emit (Offset 27) Caret "this is another another message"
          output =
            Lazy.unlines
            [ "filename:1:1: \ESC[91;1merror: \ESC[39;0mthis is a message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m1 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m\ESC[91;1m^\ESC[39;0m"
            , ""
            , "filename:1:6: \ESC[91;1merror: \ESC[39;0mthis is another message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m1 | \ESC[39;0mhere are the file contents"
            , "\ESC[34;1m  | \ESC[39;0m     \ESC[91;1m^^^\ESC[39;0m"
            , ""
            , "filename:2:1: \ESC[91;1merror: \ESC[39;0mthis is another another message"
            , "\ESC[34;1m  |\ESC[39;0m"
            , "\ESC[34;1m2 | \ESC[39;0mthis is the second line"
            , "\ESC[34;1m  | \ESC[39;0m\ESC[91;1m^\ESC[39;0m"
            ]
        input `shouldBe` output
