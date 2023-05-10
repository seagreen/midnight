module Test.MidnightBiwa.Foreign where

import Prelude

import Data.Either (Either(..))
import MidnightBiwa.Foreign as BiwaForeign
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "eval" do
    it "works" do
      BiwaForeign.eval "(+ 1 1)"
        `shouldEqual` Right "2"

    it "handles failure" do
      BiwaForeign.eval "(+ 1 1"
        `shouldEqual` Left "found EOS in list: \"(+ 1 1\" []"

  describe "compile" do
    it "works" do
      case BiwaForeign.evalToForeign "(lambda (a) a)" of
        Left e ->
          fail e

        Right l ->
          (BiwaForeign.toString <$> BiwaForeign.applyClosureFirstEvalingArgs l [ "'abc" ])
            `shouldEqual` Right "abc"

    it "handles failure" do
      case BiwaForeign.evalToForeign "(lambda (n) (+ 2 n))" of
        Left e ->
          fail e

        Right l ->
          (BiwaForeign.toString <$> BiwaForeign.applyClosureFirstEvalingArgs l [ "(1" ])
            `shouldEqual` Left "found EOS in list: \"(1\" []"
