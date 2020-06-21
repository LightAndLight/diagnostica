{-# language BangPatterns #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module Text.Diagnostic
  ( Diagnostic
  , Message(..)
  , render
  , caret
  , Text.Diagnostic.span
  )
where

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

render ::
  Text -> -- filename
  Text -> -- file contents
  Diagnostic ->
  Lazy.Text
render filePath fileContents =
  Builder.toLazyText . go 0 fileContents . Set.toAscList . unDiagnostic
  where
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
                Builder.fromText (Text.replicate (lineNumberLength + 1) (Text.singleton ' ')) <>
                Builder.fromText "|\n"
              numberedPrefix =
                lineNumberString <> Builder.fromText " | "
              unnumberedPrefix =
                Builder.fromText (Text.replicate (lineNumberLength + 1) (Text.singleton ' ')) <>
                Builder.fromText "| "

              titleLine =
                Builder.fromText filePath <> Builder.singleton ':' <>
                lineNumberString <> Builder.singleton ':' <>
                Builder.fromString (show $ dStartCol d) <> Builder.fromText ": " <>
                Builder.fromText "error: " <>
                Builder.fromText (msgTitle $ dMessage d)

              errorMessage =
                titleLine <> Builder.singleton '\n' <>
                topPrefix <>
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
