{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module Text.Diagnostic
  ( Diagnostic
  , Message(..)
  , module ANSI
  , Style(..)
  , Colors(..), defaultColors
  , Config(..), defaultConfig
  , render
  , caret
  , Text.Diagnostic.span
  )
where

import qualified System.Console.ANSI.Types as ANSI
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder

{-

thingo.file:3:22: error: short error title
   |
23 | the line on which it occurred
   |                      ^

other.file:42:22: error: short error title
   |
33 | the line on which it occurred
   |                      ^^^^^^^^

-}

data D
  = Caret
  { dLine :: {-# UNPACK #-} !Int
  , dStartCol :: {-# UNPACK #-} !Int
  , dMessage :: Message
  }
  | Span
  { dLine :: {-# UNPACK #-} !Int
  , dStartCol :: {-# UNPACK #-} !Int
  , dEndCol :: {-# UNPACK #-} !Int
  , dMessage :: Message
  }

dSize :: D -> Int
dSize Caret{} = 1
dSize (Span _ a b _) = b - a

instance Eq D where
  Caret line start msg == Caret line' start' msg' =
    line == line' && start == start' && msg == msg'
  Span line start end msg == Span line' start' end' msg' =
    line == line' && start == start' && end == end' && msg == msg'
  _ == _ = False

instance Ord D where
  compare a b =
    case compare (dLine a) (dLine b) of
      EQ ->
        case compare (dStartCol a) (dStartCol b) of
          EQ ->
            case compare (dSize a) (dSize b) of
              EQ ->
                compare (dMessage a) (dMessage a)
              x -> x
          x -> x
      x -> x

newtype Diagnostic = Diagnostic { unDiagnostic :: Set D }
  deriving (Semigroup, Monoid)

data Message
  = Message
  { msgTitle :: Text
  } deriving (Eq, Ord)

data Style
  = Style
  { consoleIntensity :: ANSI.ConsoleIntensity
  , colorIntensity :: ANSI.ColorIntensity
  , color :: ANSI.Color
  }

styleCode :: Style -> Text
styleCode style =
  case style of
    Style conI colI col ->
      case conI of
        ANSI.BoldIntensity ->
          case colI of
            ANSI.Vivid ->
              case col of
                ANSI.Black -> "\ESC[90;1m"
                ANSI.Red -> "\ESC[91;1m"
                ANSI.Green -> "\ESC[92;1m"
                ANSI.Yellow -> "\ESC[93;1m"
                ANSI.Blue -> "\ESC[94;1m"
                ANSI.Magenta -> "\ESC[95;1m"
                ANSI.Cyan -> "\ESC[96;1m"
                ANSI.White -> "\ESC[97;1m"
            ANSI.Dull ->
              case col of
                ANSI.Black -> "\ESC[30;1m"
                ANSI.Red -> "\ESC[31;1m"
                ANSI.Green -> "\ESC[32;1m"
                ANSI.Yellow -> "\ESC[33;1m"
                ANSI.Blue -> "\ESC[34;1m"
                ANSI.Magenta -> "\ESC[35;1m"
                ANSI.Cyan -> "\ESC[36;1m"
                ANSI.White -> "\ESC[37;1m"
        ANSI.FaintIntensity ->
          case colI of
            ANSI.Vivid ->
              case col of
                ANSI.Black -> "\ESC[90;2m"
                ANSI.Red -> "\ESC[91;2m"
                ANSI.Green -> "\ESC[92;2m"
                ANSI.Yellow -> "\ESC[93;2m"
                ANSI.Blue -> "\ESC[94;2m"
                ANSI.Magenta -> "\ESC[95;2m"
                ANSI.Cyan -> "\ESC[96;2m"
                ANSI.White -> "\ESC[97;2m"
            ANSI.Dull ->
              case col of
                ANSI.Black -> "\ESC[30;2m"
                ANSI.Red -> "\ESC[31;2m"
                ANSI.Green -> "\ESC[32;2m"
                ANSI.Yellow -> "\ESC[33;2m"
                ANSI.Blue -> "\ESC[34;2m"
                ANSI.Magenta -> "\ESC[35;2m"
                ANSI.Cyan -> "\ESC[36;2m"
                ANSI.White -> "\ESC[37;2m"
        ANSI.NormalIntensity ->
          case colI of
            ANSI.Vivid ->
              case col of
                ANSI.Black -> "\ESC[90m"
                ANSI.Red -> "\ESC[91m"
                ANSI.Green -> "\ESC[92m"
                ANSI.Yellow -> "\ESC[93m"
                ANSI.Blue -> "\ESC[94m"
                ANSI.Magenta -> "\ESC[95m"
                ANSI.Cyan -> "\ESC[96m"
                ANSI.White -> "\ESC[97m"
            ANSI.Dull ->
              case col of
                ANSI.Black -> "\ESC[30m"
                ANSI.Red -> "\ESC[31m"
                ANSI.Green -> "\ESC[32m"
                ANSI.Yellow -> "\ESC[33m"
                ANSI.Blue -> "\ESC[34m"
                ANSI.Magenta -> "\ESC[35m"
                ANSI.Cyan -> "\ESC[36m"
                ANSI.White -> "\ESC[37m"

data Colors
  = Colors
  { errors :: Style
  , margin :: Style
  }

defaultColors :: Colors
defaultColors =
  Colors
  { errors = Style ANSI.BoldIntensity ANSI.Vivid ANSI.Red
  , margin = Style ANSI.BoldIntensity ANSI.Dull ANSI.Blue
  }

data Config
  = Config
  { colors :: Maybe Colors
  }

defaultConfig :: Config
defaultConfig =
  Config
  { colors = Just defaultColors
  }

render ::
  Config ->
  Text -> -- filename
  Text -> -- file contents
  Diagnostic ->
  Lazy.Text
render cfg filePath fileContents =
  Builder.toLazyText . go 0 fileContents . Set.toAscList . unDiagnostic
  where
    errorsColor x =
      case colors cfg of
        Just cs ->
          Builder.fromText (styleCode $ errors cs) <>
          x <>
          Builder.fromText "\ESC[39;0m"
        Nothing ->
          x

    marginColor x =
      case colors cfg of
        Just cs ->
          Builder.fromText (styleCode $ margin cs) <>
          x <>
          Builder.fromText "\ESC[39;0m"
        Nothing ->
          x

    go :: Int -> Text -> [D] -> Builder
    go !line contents ds =
      case ds of
        [] -> mempty
        d : rest ->
          if dLine d == line
          then
            let
              (lineContents, contents') = Text.span (/= '\n') contents

              lineNumber = show line
              lineNumberLength = length lineNumber
              lineNumberString = Builder.fromString lineNumber

              topPrefix =
                marginColor $
                Builder.fromText (Text.replicate (lineNumberLength + 1) (Text.singleton ' ')) <>
                Builder.singleton '|'
              numberedPrefix =
                marginColor $ lineNumberString <> Builder.fromText " | "
              unnumberedPrefix =
                marginColor $
                Builder.fromText (Text.replicate (lineNumberLength + 1) (Text.singleton ' ')) <>
                Builder.fromText "| "

              titleLine =
                Builder.fromText filePath <> Builder.singleton ':' <>
                lineNumberString <> Builder.singleton ':' <>
                Builder.fromString (show $ dStartCol d) <> Builder.fromText ": " <>
                errorsColor (Builder.fromText "error: ") <>
                Builder.fromText (msgTitle $ dMessage d)

              errorMessage =
                titleLine <> Builder.singleton '\n' <>
                topPrefix <> Builder.singleton '\n' <>
                numberedPrefix <> Builder.fromText lineContents <> Builder.singleton '\n' <>
                unnumberedPrefix <>
                case d of
                  Caret _ dcol dmsg ->
                    Builder.fromText (Text.replicate dcol $ Text.singleton ' ') <> Builder.fromText "^\n"
                  Span _ dstartcol dendcol dmsg ->
                    Builder.fromText (Text.replicate dstartcol $ Text.singleton ' ') <>
                    Builder.fromText (Text.replicate (dendcol - dstartcol) $ Text.singleton '^') <>
                    Builder.singleton '\n'

              contents'' =
                case Text.uncons contents' of
                  Just (_, contents'') -> contents''
                  Nothing -> error "render: ran out of contents"
            in
              errorMessage <> go (line+1) contents'' rest
          else
            let
              contents' =
                case Text.uncons $ Text.dropWhile (/= '\n') contents of
                  Just (_, contents') -> contents'
                  Nothing -> error "render: ran out of contents"
            in
              go (line+1) contents' ds

caret :: Int -> Int -> Message -> Diagnostic
caret line col msg =
  Diagnostic . Set.singleton $ Caret line col msg

span :: Int -> Int -> Int -> Message -> Diagnostic
span line startCol endCol msg =
  Diagnostic . Set.singleton $ Span line startCol endCol msg
