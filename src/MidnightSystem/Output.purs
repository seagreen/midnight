module MidnightSystem.Output where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Foreign (Foreign)
import MidnightLang.Sexp (Sexp)
import MidnightSystem.Display (Display)
import MidnightSystem.Display as Display
import MidnightSystem.Util (applyStringToForeign, foreignToSexp, getSecond, getThird)

-- TODO: the word "step" is used for either 3 kinds of things in this codebase,
-- need to make it less confusing
--
-- 1. step from Lib.Moore
--
-- 2. step as in the closure stored in the ephem
--
-- 3. step from Output

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

data StepOutput = StepNormal { displaySexp :: Sexp, display :: Display, store :: Foreign, ephem :: Foreign }

jsToOutput :: Foreign -> Either String StepOutput
jsToOutput outputForeign = do
  verifyConstructor outputForeign

  store <- getSecond outputForeign
  displayForeign <-
    lmap
      (\err -> "displayFromStore: " <> err)
      (applyStringToForeign Display.fromStore store)

  displaySexp <- lmap (\err -> "foreign to display sexp: " <> err) (foreignToSexp displayForeign)
  display <- lmap (\err -> "parse display sexp: " <> err) (Display.parse displaySexp)

  ephem <- getThird outputForeign

  pure (StepNormal { displaySexp, display, store, ephem })

verifyConstructor :: Foreign -> Either String Unit
verifyConstructor output =
  void (applyStringToForeign src output)
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

(lambda (output)
  (if
    (symbol-eq? 'output-store-and-ephem (car output))
      '()
      (crash (list 'verify-constructor 'expected-output-store-and-ephem 'but-got (car output)))))

)
"""
