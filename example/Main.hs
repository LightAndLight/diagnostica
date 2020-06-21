{-# language OverloadedStrings #-}
module Main where

import Data.Text.Lazy.IO as Lazy
import Text.Diagnostic

main :: IO ()
main =
  Lazy.putStrLn $
  render defaultConfig "filename" "here are the file contents" $
    caret 0 0 (Message "this is a message") <>
    Text.Diagnostic.span 0 5 8 (Message "this is another message")
