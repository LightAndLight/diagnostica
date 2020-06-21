{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module Text.Diagnostic
  ( Report
  , Message(..)
  , Color(..)
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

data D
  = Caret
  { dLine :: {-# UNPACK #-} !Int
  , dStartCol :: {-# UNPACK #-} !Int
  , dMessage :: Message
  }
  | Span
  { dLine :: {-# UNPACK #-} !Int
  , dStartCol :: {-# UNPACK #-} !Int
  , _dEndCol :: {-# UNPACK #-} !Int
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
                compare (dMessage a) (dMessage b)
              x -> x
          x -> x
      x -> x

newtype Report = Report { unReport :: Set D }
  deriving (Semigroup, Monoid)

data Message
  = Message
  { msgTitle :: Text
  } deriving (Eq, Ord)

data Color
  = Color
  { consoleIntensity :: ANSI.ConsoleIntensity
  , colorIntensity :: ANSI.ColorIntensity
  , color :: ANSI.Color
  }

colorCode :: Color -> Text
colorCode c =
  case c of
    Color conI colI col ->
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

endColorCode :: Color -> Text
endColorCode _ = "\ESC[39;0m"

data Colors
  = Colors
  { errors :: Color
  , margin :: Color
  }

defaultColors :: Colors
defaultColors =
  Colors
  { errors = Color ANSI.BoldIntensity ANSI.Vivid ANSI.Red
  , margin = Color ANSI.BoldIntensity ANSI.Dull ANSI.Blue
  }

withColor ::
  Maybe Colors ->
  (Colors -> Color) ->
  Builder ->
  Builder
withColor mColors get b =
  case mColors of
    Nothing -> b
    Just cs ->
      let
        c = get cs
      in
        Builder.fromText (colorCode c) <>
        b <>
        Builder.fromText (endColorCode c)

data Config
  = Config
  { colors :: Maybe Colors
  , renderSpan :: Maybe Colors -> Int -> Builder
  , renderCaret :: Maybe Colors -> Builder
  }

defaultConfig :: Config
defaultConfig =
  Config
  { colors = Just defaultColors
  , renderSpan =
      \mColors len ->
        withColor mColors errors
          (Builder.fromText . Text.replicate len $ Text.singleton '^')
  , renderCaret =
      \mColors ->
        withColor mColors errors (Builder.singleton '^')
  }

render ::
  Config ->
  Text -> -- filename
  Text -> -- file contents
  Report ->
  Lazy.Text
render cfg filePath fileContents =
  let
    (lineContents, fileContents') = nextLine fileContents
  in
    Builder.toLazyText . go 0 lineContents fileContents' . Set.toAscList . unReport
  where
    nextLine cs =
      let
        (l, rest) = Text.span (/= '\n') cs
        cs' =
          maybe (error "render: ran out of contents") snd (Text.uncons rest)
      in
        (l, cs')

    errorsColor x =
      case colors cfg of
        Just cs ->
          Builder.fromText (colorCode $ errors cs) <>
          x <>
          Builder.fromText "\ESC[39;0m"
        Nothing ->
          x

    marginColor x =
      case colors cfg of
        Just cs ->
          Builder.fromText (colorCode $ margin cs) <>
          x <>
          Builder.fromText "\ESC[39;0m"
        Nothing ->
          x

    mkErrorMessage line lineContents d =
      let
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
            Caret _ dcol _ ->
              Builder.fromText (Text.replicate dcol $ Text.singleton ' ') <>
              renderCaret cfg (colors cfg) <>
              Builder.singleton '\n'
            Span _ dstartcol dendcol _ ->
              Builder.fromText (Text.replicate dstartcol $ Text.singleton ' ') <>
              renderSpan cfg (colors cfg) (dendcol - dstartcol) <>
              Builder.singleton '\n'
      in
        errorMessage

    go :: Int -> Text -> Text -> [D] -> Builder
    go !line lineContents contents ds =
      case ds of
        [] -> mempty
        [d] -> mkErrorMessage line lineContents d
        d : rest ->
          case compare line (dLine d) of
            LT ->
              let
                (lineContents', contents') = nextLine contents
              in
                go (line+1) lineContents' contents' ds
            EQ ->
              mkErrorMessage line lineContents d <> Builder.singleton '\n' <>
              go line lineContents contents rest
            GT -> mempty

caret :: Int -> Int -> Message -> Report
caret line col msg =
  Report . Set.singleton $ Caret line col msg

span :: Int -> Int -> Int -> Message -> Report
span line startCol endCol msg =
  Report . Set.singleton $ Span line startCol endCol msg
