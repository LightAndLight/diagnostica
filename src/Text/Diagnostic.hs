{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module Text.Diagnostic
  ( -- * Reports
    Report
    -- ** Configuration
    -- *** Colors
  , Color(..)
  , Colors(..), defaultColors
    -- *** General
  , Config(..), defaultConfig
    -- ** Rendering
  , render
  , renderWith
    -- * Diagnostics
  , Message(..)
  , Diagnostic(..)
  , emit
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

data Diagnostic
  = Caret
  { caretCol :: {-# UNPACK #-} !Int
  }
  | Span
  { spanFrom :: {-# UNPACK #-} !Int
  , spanTo :: {-# UNPACK #-} !Int
  } deriving Eq

diagStartCol :: Diagnostic -> Int
diagStartCol d =
  case d of
    Caret a -> a
    Span a _ -> a

data D
  = D
  { dLine :: {-# UNPACK #-} !Int
  , dSort :: Diagnostic
  , dMessage :: Message
  }

dSize :: D -> Int
dSize d =
  case dSort d of
    Caret{} -> 1
    Span a b -> b - a

dStartCol :: D -> Int
dStartCol = diagStartCol . dSort

instance Eq D where
  D dline dsort dmsg == D dline' dsort' dmsg' =
    dline == dline' &&
    dsort == dsort' &&
    dmsg == dmsg'

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
  -- | Enabled: the first line of the file is line 0, and the first column of each line is column 0
  -- Disabled: the first line of the file is line 1, and the first column of each line is column 1
  { zeroIndexed :: Bool
  , colors :: Maybe Colors
  , renderSpan :: Maybe Colors -> Int -> Builder
  , renderCaret :: Maybe Colors -> Builder
  }

defaultConfig :: Config
defaultConfig =
  Config
  { zeroIndexed = False
  , colors = Just defaultColors
  , renderSpan =
      \mColors len ->
        withColor mColors errors
          (Builder.fromText . Text.replicate len $ Text.singleton '^')
  , renderCaret =
      \mColors ->
        withColor mColors errors (Builder.singleton '^')
  }

renderWith ::
  (Int -> Text -> Diagnostic -> Message -> Builder) ->
  Int -> -- initial line number
  Text -> -- file contents
  Report ->
  Lazy.Text
renderWith mkErr lineNumber fileContents =
  let
    (lineContents, fileContents') = nextLine fileContents
  in
    Builder.toLazyText . go lineNumber lineContents fileContents' . Set.toAscList . unReport
  where
    nextLine cs =
      let
        (l, rest) = Text.span (/= '\n') cs
        cs' =
          maybe (error "render: ran out of contents") snd (Text.uncons rest)
      in
        (l, cs')

    go :: Int -> Text -> Text -> [D] -> Builder
    go !line lineContents contents ds =
      case ds of
        [] -> mempty
        [D dline dsort dmsg] ->
          case compare line dline of
            LT ->
              let
                (lineContents', contents') = nextLine contents
              in
                go (line+1) lineContents' contents' ds
            EQ ->
              mkErr line lineContents dsort dmsg
            GT -> mempty
        D dline dsort dmsg : rest ->
          case compare line dline of
            LT ->
              let
                (lineContents', contents') = nextLine contents
              in
                go (line+1) lineContents' contents' ds
            EQ ->
              mkErr line lineContents dsort dmsg <> Builder.singleton '\n' <>
              go line lineContents contents rest
            GT -> mempty

render ::
  Config ->
  Text -> -- filename
  Text -> -- file contents
  Report ->
  Lazy.Text
render cfg filePath = renderWith mkErr (if zeroIndexed cfg then 0 else 1)
  where
    errorsColor =
      case colors cfg of
        Just cs ->
          \x ->
          Builder.fromText (colorCode $ errors cs) <>
          x <>
          Builder.fromText "\ESC[39;0m"
        Nothing ->
          id

    marginColor =
      case colors cfg of
        Just cs ->
          \x ->
          Builder.fromText (colorCode $ margin cs) <>
          x <>
          Builder.fromText "\ESC[39;0m"
        Nothing ->
          id

    columnOffset =
      if zeroIndexed cfg
      then id
      else subtract 1

    mkErr :: Int -> Text -> Diagnostic -> Message -> Builder
    mkErr line lineContents d msg =
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
          Builder.fromString (show $ diagStartCol  d) <> Builder.fromText ": " <>
          errorsColor (Builder.fromText "error: ") <>
          Builder.fromText (msgTitle msg)

        errorMessage =
          titleLine <> Builder.singleton '\n' <>
          topPrefix <> Builder.singleton '\n' <>
          numberedPrefix <> Builder.fromText lineContents <> Builder.singleton '\n' <>
          unnumberedPrefix <>
          case d of
            Caret col ->
              Builder.fromText (Text.replicate (columnOffset col) $ Text.singleton ' ') <>
              renderCaret cfg (colors cfg) <>
              Builder.singleton '\n'
            Span startcol endcol ->
              Builder.fromText (Text.replicate (columnOffset startcol) $ Text.singleton ' ') <>
              renderSpan cfg (colors cfg) (endcol - startcol) <>
              Builder.singleton '\n'
      in
        errorMessage

emit :: Int -> Diagnostic -> Message -> Report
emit line sort msg =
  Report . Set.singleton $ D line sort msg
