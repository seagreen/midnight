module MidnightSystem.Step where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Foreign (Foreign)
import Lib.Moore (Moore(..))
import MidnightJS as MidnightJS
import MidnightLang.Sexp (Sexp)
import MidnightSystem.Display (Display)
import MidnightSystem.Keyboard (Keyboard)
import MidnightSystem.Keyboard as Keyboard
import MidnightSystem.Output (Output(..), StepOutput(..), jsToOutput)

stepper :: { step :: Foreign, store :: Foreign, ephem :: Foreign } -> Keyboard -> Moore Keyboard Output
stepper { step, store, ephem } k =
  case stepEither of
    Left e ->
      Moore
        { output: OutputCrash ("Eval failed: " <> e)
        , step: stepper { step, store, ephem } -- reuse old store and ephem
        }

    Right { displaySexp, display, store: newStore, ephem: newEphem } ->
      Moore
        { output: OutputSuccess { displaySexp, display, store: newStore, ephem: newEphem }
        , step: stepper { step, store: newStore, ephem: newEphem } -- new store and ephem
        }
  where
  stepEither :: Either String { displaySexp :: Sexp, display :: Display, store :: Foreign, ephem :: Foreign }
  stepEither = do
    keyboardForeign <-
      lmap
        (\err -> "Couldn't eval keyboard input: " <> err)
        (MidnightJS.evalToForeign (Keyboard.toMidnightQuoted k))

    inputMaker <-
      lmap
        (\err -> "Couldn't eval jsToInputCode: " <> err)
        (MidnightJS.evalToForeign jsToInputCode)

    inputForeign <-
      lmap
        (\err -> "Couldn't create input: " <> err)
        (MidnightJS.applyClosure inputMaker [ store, ephem, keyboardForeign ])

    val <-
      lmap
        (\err -> "Couldn't apply step: " <> err)
        (MidnightJS.applyClosure step [ inputForeign ])

    stepOutput <-
      lmap
        (\err -> "Couldn't process output: " <> err)
        (jsToOutput val)

    case stepOutput of
      StepNormal { displaySexp, display, store: newStore, ephem: newEphem } ->
        pure { displaySexp, display, store: newStore, ephem: newEphem }

jsToInputCode :: String
jsToInputCode =
  """
(lambda (store ephem k)
  (let
    ((list (lambda xs xs)))
    (list 'system-input-keypress store ephem k)))
"""
