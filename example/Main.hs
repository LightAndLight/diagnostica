{-# language OverloadedStrings #-}
module Main where

import Text.Diagnostic
import qualified Data.ByteString.Lazy.Char8 as LazyByteString.Char8
import qualified Data.ByteString.Lazy as LazyByteString

main :: IO ()
main =
  let
    line1 = "here are the file contents"
    line2 = "this is the second line"
    line3 = "this is the third line"
  in
    LazyByteString.Char8.putStrLn $
    render defaultConfig "filename" (LazyByteString.Char8.unlines [line1, line2, line3]) $
      emit (Pos 1 1) Caret "this is a message" <>
      emit (Pos 1 6) (Span 3) "this is another message" <>
      emit (Pos 2 6) Caret "this is the third message" <>
      emit (Offset $ fromIntegral (LazyByteString.length line1) + fromIntegral (LazyByteString.length line2) + 2 + 12) Caret "this is the fourth message"
