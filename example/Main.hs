{-# language OverloadedStrings #-}
module Main where

import Data.Text.Lazy.IO as Lazy
import Text.Diagnostic

main :: IO ()
main =
  Lazy.putStrLn $
  render defaultConfig "filename" "here are the file contents\nthis is the second line" $
    emit 1 (Caret 1) (Message "this is a message") <>
    emit 1 (Span 6 9) (Message "this is another message") <>
    emit 2 (Caret 6) (Message "this is the third message")
