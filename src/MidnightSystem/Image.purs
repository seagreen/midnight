module MidnightSystem.Image where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Int as Int
import Data.List ((:))
import Data.List as PsList
import Data.Traversable (for)
import Lib.Image (Image)
import Lib.Image as Image
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

parse :: Sexp -> Either String Image
parse = case _ of
  Sexp.Symbol "image-empty" ->
    pure mempty

  Sexp.List (Sexp.Symbol "image-multiple" : Sexp.List xs : PsList.Nil) ->
    fold <$> for xs parse

  Sexp.List (Sexp.Symbol "image-line" : Sexp.Int x1 : Sexp.Int y1 : Sexp.Int x2 : Sexp.Int y2 : PsList.Nil) ->
    pure
      ( Image.line
          { x: Int.toNumber x1, y: Int.toNumber y1 }
          { x: Int.toNumber x2, y: Int.toNumber y2 }
      )

  Sexp.List (Sexp.Symbol "image-string" : Sexp.Int x : Sexp.Int y : stringAsTaggedListOfInts : PsList.Nil) -> do
    str <- Sexp.midnightStringToPureScriptString stringAsTaggedListOfInts
    pure
      ( Image.translate
          { x: Int.toNumber x, y: Int.toNumber y }
          (Image.text str)
      )

  other ->
    Left ("Parse image: part not recognized: " <> Sexp.print other)
