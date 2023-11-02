module MidnightSystem.Util where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Foreign (Foreign, unsafeFromForeign)
import MidnightJS as MidnightJS
import MidnightJS.JsonToMidnightSexp (jsonToMidnightSexp)
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

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
