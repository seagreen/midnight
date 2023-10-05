module MidnightSystem.Display where

import Prelude

import Data.Array (fromFoldable)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Int as Int
import Data.List (List)
import Data.List ((:))
import Data.List as PsList
import Data.String as String
import Data.Traversable (for)
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

displayWidth :: Int
displayWidth =
  80

displayHeight :: Int
displayHeight =
  30

type Display =
  { cursorPosition :: { x :: Int, y :: Int }
  , text :: Array String
  }

parse :: Sexp -> Either String Display
parse =
  case _ of
    Sexp.List (cursorPositionSexp : textSexp : PsList.Nil) -> do
      cursorPosition <- parseCursorPosition cursorPositionSexp
      text <- clipToFit <$> parseText textSexp
      pure { cursorPosition: cursorPosition, text: text }

    other ->
      Left ("Display sexp top level not a two-element list, expected cursor position and text: " <> Sexp.print other)

parseCursorPosition :: Sexp -> Either String { x :: Int, y :: Int }
parseCursorPosition =
  case _ of
    Sexp.List (Sexp.Symbol "cursor-position" : Sexp.Int x : Sexp.Int y : PsList.Nil) -> do
      Right { x, y }

    other ->
      Left ("Display cursor-position incorrect: " <> Sexp.print other)

parseText :: Sexp -> Either String (Array String)
parseText =
  case _ of
    Sexp.List (Sexp.Symbol "text" : Sexp.List lines : PsList.Nil) ->
      fromFoldable <$> for lines Sexp.midnightStringToPureScriptString

    other ->
      Left ("Display text field incorrect: " <> Sexp.print other)

clipToFit :: Array String -> Array String
clipToFit xs =
  String.take displayWidth <$> Array.take displayHeight xs
