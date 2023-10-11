module MidnightSystem where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Lib.Moore (Moore(..))
import MidnightJS as MidnightJS
import MidnightLang.Sexp (Sexp)
import MidnightSystem.Display (Display)
import MidnightSystem.Keyboard (Keyboard)
import MidnightSystem.Keyboard as Keyboard
import MidnightSystem.Output (StepOutput(..), jsToOutput)
import MidnightSystem.Startup as Startup

data Output
  = OutputCrash String
  | OutputSuccess { displaySexp :: Sexp, display :: Display, store :: Foreign, ephem :: Foreign }

instance Show Output where
  -- | For tests
  show :: Output -> String
  show = case _ of
    OutputCrash err ->
      "OutputCrash: " <> err

    OutputSuccess _ ->
      "<OutputSuccess>"

newtype StartupFailure = StartupFailure String

derive instance Generic StartupFailure _
instance Show StartupFailure where
  show a = genericShow a

moore :: String -> Either StartupFailure (Moore Keyboard Output)
moore midnightSrc =
  lmap StartupFailure do
    -- Evaluate our Midnight source code, turning it into a JS closure.
    --
    -- (this is slow)
    mainMidnight <-
      lmap
        (\err -> "Evaluation of source failed: " <> err)
        (MidnightJS.evalToForeign midnightSrc)

    -- Starting with the original Midnight source again,
    -- turn it into a JS value containing a Midnight string.
    startingInput <-
      lmap
        (\err -> "Evaluation of starting input failed: " <> err)
        (Startup.editorStringToInput midnightSrc)

    -- Apply the JS closure to the Midnight string.
    --
    -- (this is slow too)
    systemOutput <-
      lmap
        (\err -> "Application of the main function failed: " <> err)
        (MidnightJS.applyClosure mainMidnight [ startingInput ])

    -- Parse the output.
    StepNormal { displaySexp, display, store, ephem } <-
      lmap
        (\err -> "Processing of the output failed: " <> err)
        (jsToOutput systemOutput)

    pure
      ( Moore
          { output: OutputSuccess { displaySexp, display, store, ephem }
          , step: stepper { step: mainMidnight, store, ephem }
          }
      )

-- * step

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
        (MidnightJS.evalToForeign (Keyboard.toJS k))

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
