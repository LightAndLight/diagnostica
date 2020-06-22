{-# language OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Lazy
import Text.Diagnostic

main :: IO ()
main =
  let
    line1 = "here are the file contents"
    line2 = "this is the second line"
    line3 = "this is the third line"
  in
    Lazy.putStrLn $
    render defaultConfig "filename" (Text.unlines [line1, line2, line3]) $
      emit (Pos 1 1) Caret (Message "this is a message") <>
      emit (Pos 1 6) (Span 3) (Message "this is another message") <>
      emit (Pos 2 6) Caret (Message "this is the third message") <>
      emit (Offset $ Text.length line1 + Text.length line2 + 2 + 12) Caret (Message "this is the fourth message")
