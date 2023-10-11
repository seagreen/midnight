module MidnightSystem.Output where

import Lib.Debug
import Prelude
import Debug

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
import MidnightJS.JsonToMidnightSexp as JsonToMidnightSexp
import MidnightJS.Translate as Translate
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightJS.JsonToMidnightSexp (jsonToMidnightSexp)
import MidnightSystem.Display (Display)
import MidnightSystem.Display as Display

data StepOutput = StepNormal { displaySexp :: Sexp, display :: Display, store :: Foreign, ephem :: Foreign }

jsToOutput :: Foreign -> Either String StepOutput
jsToOutput val = do
  {-
  outputConstructorForeign <- getFirst val
  outputConstructorSexp <- foreignToSexp outputConstructorForeign
  case outputConstructorSexp of
    Sexp.Symbol "output-normal" -> do

-}
  displayForeign <- getSecond val
  displaySexp <- lmap (\err -> "foreign to display sexp: " <> err) (foreignToSexp displayForeign)
  display <- lmap (\err -> "parse display sexp: " <> err) (Display.parse displaySexp)
  store <- getThird val
  ephem <- getFourth val
  pure (StepNormal { displaySexp, display, store, ephem })

{-
    other ->
      Left ("Parse step output: output constructor not recognized: " <> Sexp.print other)
-}

foreignToSexp :: Foreign -> Either String Sexp
foreignToSexp jsVal = do
  jsonToMidnightSexp (unsafeFromForeign jsVal)

foreignToSexpAllowClosures :: Foreign -> Either String Sexp
foreignToSexpAllowClosures jsVal = do
  pure (Sexp.Symbol "TODO TODO")

-- genericSexp <- GenericSexp.parse (Foreign.String jsVal)
-- crash "here"

-- Translate.biwaSexpToMidnightSexp biwaSexp

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

getFourth :: Foreign -> Either String Foreign
getFourth jsVal =
  lmap
    (\err -> "getFourth: " <> err)
    (applyStringToForeign "(lambda (x) (car (cdr (cdr (cdr x)))))" jsVal)

applyStringToForeign :: String -> Foreign -> Either String Foreign
applyStringToForeign str jsVal = do
  f <- MidnightJS.evalToForeign str
  MidnightJS.applyClosure f [ jsVal ]
