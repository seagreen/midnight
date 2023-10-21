module MidnightSystem.StoreToMoore where

-- TODO: this depends on MidnightSystem, so we're flipped around

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
import MidnightSystem (stepper)
import MidnightSystem.Display as Display
import MidnightSystem.Keyboard (Keyboard)
import MidnightSystem.Output (Output(..), applyStringToForeign, displayFromStore, foreignToSexp)

storeToMoore :: String -> Either String (Moore Keyboard Output)
storeToMoore storeSexpString = do

  -- Can't actually do this hack, get:
  --
  -- > RangeError: Maximum call stack size exceeded
  --
  -- storeForeign <- MidnightJS.evalToForeign ("(quote " <> storeSexpString <> ")") -- NOTE: hack

  storeForeign <- sexpStrToJson storeSexpString

  displayForeign <-
    lmap
      (\err -> "displayFromStore: " <> err)
      (applyStringToForeign displayFromStore storeForeign)

  displaySexp <- lmap (\err -> "foreign to display sexp: " <> err) (foreignToSexp displayForeign)
  display <- lmap (\err -> "parse display sexp: " <> err) (Display.parse displaySexp)

  storeToEphemInput <- storeToInput storeForeign

  mainSexpForeign <- mainSexpFromStore storeForeign

  mainEvaled <- applyStringToForeign "eval" mainSexpForeign

  ephem <- MidnightJS.applyClosure mainEvaled [ storeToEphemInput ]
  pure
    ( Moore
        { output: OutputSuccess { displaySexp, display, store: storeForeign, ephem }
        , step: stepper { step: mainEvaled, store: storeForeign, ephem }
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
