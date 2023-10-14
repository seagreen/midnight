module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Lib.Sexp as Test.Lib.Sexp
import Test.MidnightJS as Test.MidnightJS
import Test.MidnightSystem as Test.MidnightSystem
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] do
    describe "sexpr" Test.Lib.Sexp.spec
    describe "midnight-js" Test.MidnightJS.spec
    describe "midnight-vm" Test.MidnightSystem.spec
