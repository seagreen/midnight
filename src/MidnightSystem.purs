module MidnightSystem where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign (Foreign)
import Lib.Image (Image)
import Lib.Moore (Moore(..))
import MidnightBiwa as MidnightBiwa
import MidnightLang.Sexp (Sexp)
import MidnightSystem.Keyboard (Keyboard)
import MidnightSystem.Keyboard as Keyboard
import MidnightSystem.Output (StepOutput(..), biwaOutput)
import MidnightSystem.Startup as Startup

data Output
  = OutputCrash String
  | OutputSuccess { imageSexp :: Sexp, image :: Image, store :: Foreign, ephem :: Foreign }

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
    mainMidnight <-
      lmap
        (\err -> "Evaluation of source failed: " <> err)
        (MidnightBiwa.evalToForeign midnightSrc)

    startingInput <-
      lmap
        (\err -> "Evaluation of starting input failed: " <> err)
        (Startup.editorStringToInput midnightSrc)

    systemOutput <-
      lmap
        (\err -> "Application of the main function failed: " <> err)
        (MidnightBiwa.applyClosure mainMidnight [ startingInput ])

    StepNormal { imageSexp, image, store, ephem } <-
      lmap
        (\err -> "Processing of the output failed: " <> err)
        (biwaOutput systemOutput)

    pure
      ( Moore
          { output: OutputSuccess { imageSexp, image, store, ephem }
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

    Right { imageSexp, image, store: newStore, ephem: newEphem } ->
      Moore
        { output: OutputSuccess { imageSexp, image, store: newStore, ephem: newEphem }
        , step: stepper { step, store: newStore, ephem: newEphem } -- new store and ephem
        }
  where
  stepEither :: Either String { imageSexp :: Sexp, image :: Image, store :: Foreign, ephem :: Foreign }
  stepEither = do
    keyboardForeign <-
      lmap
        (\err -> "Couldn't eval keyboard input: " <> err)
        (MidnightBiwa.evalToForeign (Keyboard.toBiwa k))

    inputMaker <-
      lmap
        (\err -> "Couldn't eval biwaToInputCode: " <> err)
        (MidnightBiwa.evalToForeign biwaToInputCode)

    inputForeign <-
      lmap
        (\err -> "Couldn't create input: " <> err)
        (MidnightBiwa.applyClosure inputMaker [ store, ephem, keyboardForeign ])

    val <-
      lmap
        (\err -> "Couldn't apply step: " <> err)
        (MidnightBiwa.applyClosure step [ inputForeign ])

    stepOutput <-
      lmap
        (\err -> "Couldn't process output: " <> err)
        (biwaOutput val)

    case stepOutput of
      StepNormal { imageSexp, image, store: newStore, ephem: newEphem } ->
        pure { imageSexp, image, store: newStore, ephem: newEphem }

biwaToInputCode :: String
biwaToInputCode =
  """
(lambda (store ephem k)
  (let
    ((list (lambda xs xs)))
    (list 'system-input-keypress store ephem k)))
"""
