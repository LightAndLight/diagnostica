{-# language BangPatterns #-}
{-# language OverloadedStrings #-}
{-# language UnboxedSums, UnboxedTuples #-}
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
    -- *** Custom rendering
  , Layout(..)
  , renderWith
    -- * Diagnostics
  , Position(..)
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
import Data.Text.Internal (Text(Text), text)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Unsafe (Iter(Iter), iter)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder

data Diagnostic
  = Caret
  | Span {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

data Position
  = Offset {-# UNPACK #-} !Int
  | Pos
    {-# UNPACK #-} !Int -- line number
    {-# UNPACK #-} !Int -- column number
  deriving (Eq, Ord)

data Positioned
  = Positioned
    {-# UNPACK #-} !Int -- line
    {-# UNPACK #-} !Int -- col
    Diagnostic
    Message
  deriving (Eq, Ord, Show)

data Offseted
  = Offseted
    {-# UNPACK #-} !Int -- offset
    Diagnostic
    Message
  deriving (Eq, Ord, Show)

data Report
  = Report
  { reportPositioned :: Set Positioned
  , reportOffseted :: Set Offseted
  }

instance Semigroup Report where; Report a b <> Report a' b' = Report (a <> a') (b <> b')
instance Monoid Report where; mempty = Report mempty mempty

data Message
  = Message
  { msgTitle :: Text
  } deriving (Eq, Ord, Show)

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

-- | Calculate the `span` of the input `Text` and additionally return the length of
-- the prefix in `Char`s
spanWithLength :: (Char -> Bool) -> Text -> (Int, Text, Text)
spanWithLength p t@(Text arr off len) = (hdLen, hd, tl)
  where
    hd = text arr off k
    tl = text arr (off+k) (len-k)
    (# hdLen, !k #) = loop 0 0
    loop !l !i
      | i < len && p c = loop (l+1) (i+d)
      | otherwise = (# l, i #)
      where
        Iter c d = iter t i

data Layout
  = Layout
  -- | Create enough padding to reach a particular column
  { padUntil :: Int -> Builder
  , lineNumber :: Int
  , columnNumber :: Int
  , currentLine :: Text
  }

renderWith ::
  (Layout -> Diagnostic -> Message -> Builder) ->
  Config ->
  Text -> -- file contents
  Report ->
  Lazy.Text
renderWith mkErr cfg fileContents (Report positions offsets) =
  let
    (lineContentsLen, lineContents, fileContents') = nextLine fileContents
  in
    Builder.toLazyText $
    go
      zeroIndexedOffset
      0
      zeroIndexedOffset
      lineContents
      lineContentsLen
      fileContents'
      (Set.toAscList positions)
      (Set.toAscList offsets)
  where
    zeroIndexedOffset = if zeroIndexed cfg then 0 else 1

    padUntilFun =
      if zeroIndexed cfg
      then \col -> Builder.fromText $ Text.replicate col (Text.singleton ' ')
      else \col -> Builder.fromText $ Text.replicate (col-1) (Text.singleton ' ')

    mkLayout l c cl =
      Layout
      { padUntil = padUntilFun
      , lineNumber = l
      , columnNumber = c
      , currentLine = cl
      }

    nextLine cs =
      let
        (lLen, l, rest) = spanWithLength (/= '\n') cs
        cs' =
          maybe (error "render: ran out of contents") snd (Text.uncons rest)
      in
        -- either span finds a newline, or it consumes the whole string
        -- in the former case, the +1 allows offsets to point to the newline character of a line
        -- in the latter, it allows offsets to point to the end of the file
        ( lLen + 1
        , l
        , cs'
        )

    fetchOffseted1 colOffset lineStartOffset lineContentsLen line ps o@(Offseted off osort omsg) rest =
      let
        localOff = off - lineStartOffset
      in
        if 0 <= localOff
        then
          if localOff < lineContentsLen
          then -- the offset is on this line
            (# | | (# ps, rest, Positioned line (localOff + colOffset) osort omsg #) #)
          else -- the offset is on a later line
            (# | () | #)
        else
          error $ "internal error: " <> show o <> " was left behind"

    fetchOffseted ::
      Int ->
      Int ->
      Int ->
      Int ->
      [Positioned] ->
      [Offseted] ->
      (#
        () | -- finished
        () | -- continue
        (# [Positioned], [Offseted], Positioned #) -- consumed
      #)
    fetchOffseted colOffset lineStartOffset lineContentsLen line ps os =
      case os of
        [] -> (# | () | #)
        o : rest -> fetchOffseted1 colOffset lineStartOffset lineContentsLen line ps o rest

    fetch ::
      Int ->
      Int ->
      Int ->
      Int ->
      [Positioned] ->
      [Offseted] ->
      (#
        () | -- finished
        () | -- continue
        (# [Positioned], [Offseted], Positioned #) -- consumed
      #)
    fetch colOffset lineStartOffset lineContentsLen line ps os =
      case ps of
        [] ->
          case os of
            [] -> (# () | | #)
            o : rest -> fetchOffseted1 colOffset lineStartOffset lineContentsLen line ps o rest
        p@(Positioned pline _ _ _) : rest ->
          case compare pline line of
            LT ->
              error $ "internal error: " <> show p <> " was left behind"
            EQ -> (# | | (# rest, os, p #) #)
            GT ->
              fetchOffseted colOffset lineStartOffset lineContentsLen line ps os

    go :: Int -> Int -> Int -> Text -> Int -> Text -> [Positioned] -> [Offseted] -> Builder
    go !colOffset !lineStartOffset !line lineContents lineContentsLen contents ps os =
      case fetch colOffset lineStartOffset lineContentsLen line ps os of
        (# () | | #) -> mempty
        (# | () | #) ->
          let
            (lineContentsLen', lineContents', contents') = nextLine contents
          in
            go colOffset (lineStartOffset+lineContentsLen) (line+1) lineContents' lineContentsLen' contents' ps os
        (# | | (# ps', os', Positioned pline pcol psort pmsg #) #) ->
          if null ps' && null os'
          then
            mkErr (mkLayout pline pcol lineContents) psort pmsg
          else
            mkErr (mkLayout pline pcol lineContents) psort pmsg <> Builder.singleton '\n' <>
            go colOffset lineStartOffset line lineContents lineContentsLen contents ps' os'

render ::
  Config ->
  Text -> -- filename
  Text -> -- file contents
  Report ->
  Lazy.Text
render cfg filePath = renderWith mkErr cfg
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

    mkErr :: Layout -> Diagnostic -> Message -> Builder
    mkErr layout d msg =
      let
        showLineNumber = show $ lineNumber layout
        lineNumberLength = length showLineNumber
        lineNumberString = Builder.fromString showLineNumber

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
          Builder.fromString (show $ columnNumber layout) <> Builder.fromText ": " <>
          errorsColor (Builder.fromText "error: ") <>
          Builder.fromText (msgTitle msg)

        col = columnNumber layout
        errorMessage =
          titleLine <> Builder.singleton '\n' <>
          topPrefix <> Builder.singleton '\n' <>
          numberedPrefix <> Builder.fromText (currentLine layout) <> Builder.singleton '\n' <>
          unnumberedPrefix <>
          case d of
            Caret ->
              padUntil layout col <>
              renderCaret cfg (colors cfg) <>
              Builder.singleton '\n'
            Span len ->
              padUntil layout col <>
              renderSpan cfg (colors cfg) len <>
              Builder.singleton '\n'
      in
        errorMessage

emit :: Position -> Diagnostic -> Message -> Report
emit pos sort msg =
  case pos of
    Offset off ->
      Report { reportPositioned = mempty, reportOffseted = Set.singleton $ Offseted off sort msg }
    Pos l c ->
      Report { reportPositioned = Set.singleton $ Positioned l c sort msg, reportOffseted = mempty }
