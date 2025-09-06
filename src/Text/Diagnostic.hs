{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language UnboxedTuples #-}
module Text.Diagnostic
  ( Report
    -- * Configuration
    -- ** Colors
  , Color(..)
  , Colors(..), defaultColors
    -- ** General
  , Config(..), defaultConfig
  , Message(..)
    -- * Rendering
  , render
    -- ** Custom rendering
  , Layout(..)
  , renderWith
    -- * Diagnostics
  , Position(..)
  , Diagnostic(..)
  , emit
  )
where

import qualified System.Console.ANSI.Types as ANSI
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.String (IsString, fromString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Word (Word8)
import qualified Data.ByteString as ByteString
import Data.Char (ord)
import qualified Data.ByteString.Char8 as ByteString.Char8

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

newtype Message = Message { unMessage :: Builder }
  deriving (Show, IsString)

instance Eq Message where
  Message a == Message b = Builder.toLazyByteString a == Builder.toLazyByteString b

instance Ord Message where
  Message a `compare` Message b = Builder.toLazyByteString a `compare` Builder.toLazyByteString b

data Color
  = Color
  { consoleIntensity :: ANSI.ConsoleIntensity
  , colorIntensity :: ANSI.ColorIntensity
  , color :: ANSI.Color
  }

colorCode :: Color -> ByteString
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

endColorCode :: Color -> ByteString
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
        Builder.byteString (colorCode c) <>
        b <>
        Builder.byteString (endColorCode c)

data Config
  = Config
  { -- | Enabled: the first line of the file is line 0, and the first column of each line is column 0
    --
    -- Disabled: the first line of the file is line 1, and the first column of each line is column 1
    zeroIndexed :: Bool
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
          (Builder.byteString $ ByteString.Char8.replicate len '^')
  , renderCaret =
      \mColors ->
        withColor mColors errors (Builder.byteString "^")
  }

-- | Equivalent to @\f x -> let (prefix, rest) = span f xs in (length prefix, prefix, rest)@
spanWithLength :: (Word8 -> Bool) -> Lazy.ByteString -> (Int, ByteString, Lazy.ByteString)
spanWithLength p bs =
  let
    (prefixLazy, rest) = ByteString.Lazy.span p bs
    !prefix = ByteString.Lazy.toStrict prefixLazy
    !prefixLen = ByteString.length prefix
  in
    (prefixLen, prefix, rest)

data Layout
  = Layout
  { -- | Create enough padding to reach a particular column
    padUntil :: Int -> Builder
  , lineNumber :: Int
  , columnNumber :: Int
  , currentLine :: ByteString
  }

renderWith ::
  (Layout -> Diagnostic -> Message -> Builder) ->
  Config ->
  -- | File contents
  Lazy.ByteString ->
  Report ->
  Lazy.ByteString
renderWith mkErr cfg fileContents (Report positions offsets) =
  let
    (lineContentsLen, lineContents, fileContents') = nextLine fileContents
  in
    Builder.toLazyByteString $
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
      then \col -> Builder.byteString $ ByteString.Char8.replicate col ' '
      else \col -> Builder.byteString $ ByteString.Char8.replicate (col-1) ' '

    mkLayout ::
      -- | Line
      Int ->
      -- | Column
      Int ->
      -- | Current line
      ByteString ->
      Layout
    mkLayout l c cl =
      Layout
      { padUntil = padUntilFun
      , lineNumber = l
      , columnNumber = c
      , currentLine = cl
      }

    nextLine ::
      Lazy.ByteString ->
      ( Int
      -- line
      , ByteString
      -- remaining
      , Lazy.ByteString
      )
    nextLine cs =
      let
        (lLen, l, rest) = spanWithLength (/= fromIntegral (ord '\n')) cs
        cs' =
          maybe (error "render: ran out of contents") snd (ByteString.Lazy.uncons rest)
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

    go ::
      -- | Column offset
      Int ->
      -- | Line start offset
      Int ->
      -- | Line
      Int ->
      -- | Line contents
      ByteString ->
      -- | Line contents length
      Int ->
      -- | File contents
      Lazy.ByteString ->
      [Positioned] ->
      [Offseted] ->
      Builder
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
            mkErr (mkLayout pline pcol lineContents) psort pmsg <> Builder.byteString "\n" <>
            go colOffset lineStartOffset line lineContents lineContentsLen contents ps' os'

-- | Constructs a lazy 'Lazy.ByteString' containing one formatted diagnostic message
-- for each item in the 'Report'.
render ::
  Config ->
  -- | File name
  ByteString -> 
  -- | File contents
  Lazy.ByteString ->
  Report ->
  Lazy.ByteString
render cfg filePath = renderWith mkErr cfg
  where
    errorsColor =
      case colors cfg of
        Just cs ->
          \x ->
          Builder.byteString (colorCode $ errors cs) <>
          x <>
          "\ESC[39;0m"
        Nothing ->
          id

    marginColor =
      case colors cfg of
        Just cs ->
          \x ->
          Builder.byteString(colorCode $ margin cs) <>
          x <>
          "\ESC[39;0m"
        Nothing ->
          id

    mkErr :: Layout -> Diagnostic -> Message -> Builder
    mkErr layout d msg =
      let
        showLineNumber = show $ lineNumber layout
        lineNumberLength = length showLineNumber
        lineNumberString = fromString showLineNumber

        topPrefix =
          marginColor $
          Builder.byteString (ByteString.Char8.replicate (lineNumberLength + 1) ' ') <>
          Builder.byteString "|"
        numberedPrefix =
          marginColor $ lineNumberString <> Builder.byteString " | "
        unnumberedPrefix =
          marginColor $
          Builder.byteString (ByteString.Char8.replicate (lineNumberLength + 1) ' ') <>
          Builder.byteString "| "

        titleLine =
          Builder.byteString filePath <> Builder.byteString ":" <>
          lineNumberString <> Builder.byteString ":" <>
          fromString (show $ columnNumber layout) <> Builder.byteString ": " <>
          errorsColor (Builder.byteString "error: ") <>
          unMessage msg

        col = columnNumber layout
        errorMessage =
          titleLine <> Builder.char8 '\n' <>
          topPrefix <> Builder.char8 '\n' <>
          numberedPrefix <> Builder.byteString (currentLine layout) <> Builder.char8 '\n' <>
          unnumberedPrefix <>
          case d of
            Caret ->
              padUntil layout col <>
              renderCaret cfg (colors cfg) <>
              Builder.char8 '\n'
            Span len ->
              padUntil layout col <>
              renderSpan cfg (colors cfg) len <>
              Builder.char8 '\n'
      in
        errorMessage

emit ::
  Position ->
  Diagnostic ->
  Message ->
  Report
emit pos sort msg =
  case pos of
    Offset off ->
      Report
      { reportPositioned = mempty
      , reportOffseted = Set.singleton $ Offseted off sort msg
      }
    Pos l c ->
      Report
      { reportPositioned = Set.singleton $ Positioned l c sort msg
      , reportOffseted = mempty
      }
