{-# language OverloadedStrings #-}
module Main where

import Data.Text.Lazy.IO as Lazy
import Text.Diagnostic

main :: IO ()
main =
  Lazy.putStrLn $
  render defaultConfig "filename" "here are the file contents" $
    emit 0 (Caret 0) (Message "this is a message") <>
    emit 0 (Span 5 8) (Message "this is another message")
