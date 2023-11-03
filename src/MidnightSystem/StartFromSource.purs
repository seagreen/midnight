module MidnightSystem.StartFromSource
  ( startFromSource
  ) where

import Prelude

import Data.Bifunctor (bimap, lmap)
import Data.Either (Either)
import Foreign (Foreign, unsafeToForeign)
import Lib.Moore (Moore(..))
import MidnightJS as MidnightJS
import MidnightSystem.Keyboard (Keyboard)
import MidnightSystem.Output (Output(..), StepOutput(..), jsToOutput)
import MidnightSystem.Step (stepper)
import MidnightSystem.SystemForeign as SystemForeign

startFromSource :: String -> Either String (Moore Keyboard Output)
startFromSource midnightSrc = do
  -- Evaluate our Midnight source code, turning it into a JS closure.
  --
  -- This takes about 1s.
  mainMidnight <-
    lmap
      (\err -> "Evaluation of source failed: " <> err)
      (MidnightJS.evalToForeign midnightSrc)

  -- Starting with the original Midnight source again,
  -- turn it into a JS value containing a Midnight string.
  startingInput <-
    lmap
      (\err -> "Evaluation of starting input failed: " <> err)
      (editorStringToInput midnightSrc)

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

editorStringToInput :: String -> Either String Foreign
editorStringToInput sourceStrPs = do
  bimap
    (\err -> "Evaluation of editorStringToInput failed: " <> err)
    unsafeToForeign
    (SystemForeign.editorStringToInput sourceStrPs)
