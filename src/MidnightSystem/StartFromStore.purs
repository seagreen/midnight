module MidnightSystem.StartFromStore
  ( startFromStoreText
  ) where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int as Int
import Data.List as List
import Foreign (Foreign, unsafeToForeign)
import Lib.Moore (Moore(..))
import MidnightJS as MidnightJS
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem.Display as Display
import MidnightSystem.Keyboard (Keyboard)
import MidnightSystem.Output (Output(..))
import MidnightSystem.Step (stepper)
import MidnightSystem.Util (applyStringToForeign, foreignToSexp, getSecond, verifyConstructor)

startFromStoreText :: String -> Either String (Moore Keyboard Output)
startFromStoreText storeSexpString = do

  -- Can't actually do this hack, get:
  --
  -- > RangeError: Maximum call stack size exceeded
  --
  -- storeForeign <- MidnightJS.evalToForeign ("(quote " <> storeSexpString <> ")") -- NOTE: hack

  store <- sexpStrToJson storeSexpString

  displayForeign <- Display.fromStore store
  displaySexp <- lmap (\err -> "foreign to display sexp: " <> err) (foreignToSexp displayForeign)
  display <- lmap (\err -> "parse display sexp: " <> err) (Display.parse displaySexp)

  storeToEphemInput <- storeToInput store

  mainSexpForeign <- mainSexpFromStore store

  mainEvaled <- applyStringToForeign "eval" mainSexpForeign

  output <- MidnightJS.applyClosure mainEvaled [ storeToEphemInput ]
  verifyConstructor "output-ephem" output
  ephem <- getSecond output

  pure
    ( Moore
        { output: OutputSuccess { displaySexp, display, store, ephem }
        , step: stepper { step: mainEvaled, store, ephem }
        }
    )

sexpStrToJson :: String -> Either String Foreign
sexpStrToJson sexpStr = do
  sexp <- Sexp.parse sexpStr
  pure (unsafeToForeign (sexpToJson sexp))

sexpToJson :: Sexp -> Json
sexpToJson =
  case _ of
    Sexp.Symbol sym ->
      Json.fromString sym

    Sexp.List xs ->
      case xs of
        List.Nil ->
          Json.fromArray []

        List.Cons x rest ->
          -- (Array.fromFoldable (sexpToJson <$> xs))
          Json.fromArray
            [ sexpToJson x
            , sexpToJson (Sexp.List rest)
            ]

    Sexp.Int i ->
      Json.fromNumber (Int.toNumber i)

storeToInput :: Foreign -> Either String Foreign
storeToInput store =
  applyStringToForeign src store
  where
  src :: String
  src =
    """
(let
  (

(list
  (lambda xs
    xs))

)

(lambda (store)
  (list 'store-to-ephem store))

)
"""

mainSexpFromStore :: Foreign -> Either String Foreign
mainSexpFromStore output =
  applyStringToForeign src output
  where
  src :: String
  src =
    """
(let
  (

(list
  (lambda xs
    xs))

(untagged-alist-get-symbol
  (lambda (sym xs)
    (if
      (list-empty? xs)
      (crash (list 'untagged-alist-get-symbol 'not-found sym))
      (if
        (symbol-eq? sym (car (car xs)))
        (car (cdr (car xs)))
        (untagged-alist-get-symbol sym (cdr xs))))))

)

(lambda (store)
  (untagged-alist-get-symbol 'main-sexp store))

)
"""
