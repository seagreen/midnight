module MidnightSystem.Util where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodePoints (CodePoint)
import Data.Traversable (for)
import Foreign (Foreign, unsafeFromForeign)
import Lib.Sexp as GenericSexp
import MidnightJS as MidnightJS
import MidnightJS.Foreign as Foreign
import MidnightJS.JsonToMidnightSexp (jsonToMidnightSexp)
import MidnightJS.JsonToMidnightSexp as JsonToMidnightSexp
import MidnightJS.Translate as Translate
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem.Display (Display)
import MidnightSystem.Display as Display

getFirst :: Foreign -> Either String Foreign
getFirst val =
  lmap
    (\err -> "getFirst: " <> err)
    (applyStringToForeign "(lambda (x) (car x))" val)

getSecond :: Foreign -> Either String Foreign
getSecond jsVal =
  lmap
    (\err -> "getSecond: " <> err)
    (applyStringToForeign "(lambda (x) (car (cdr x)))" jsVal)

getThird :: Foreign -> Either String Foreign
getThird jsVal =
  lmap
    (\err -> "getThird: " <> err)
    (applyStringToForeign "(lambda (x) (car (cdr (cdr x))))" jsVal)

applyStringToForeign :: String -> Foreign -> Either String Foreign
applyStringToForeign str jsVal = do
  f <- MidnightJS.evalToForeign str
  MidnightJS.applyClosure f [ jsVal ]

foreignToSexp :: Foreign -> Either String Sexp
foreignToSexp jsVal = do
  jsonToMidnightSexp (unsafeFromForeign jsVal)

foreignToSexpAllowClosures :: Foreign -> Either String Sexp
foreignToSexpAllowClosures jsVal = do
  pure (Sexp.Symbol "TODO TODO")
