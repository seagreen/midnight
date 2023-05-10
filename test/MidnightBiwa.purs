module Test.MidnightBiwa where

import Prelude

import Data.Either (Either(..))
import MidnightBiwa as Biwa
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "midnight evaluated through biwa" do
    it "lambda" do
      Biwa.eval "((lambda (n) (+ n 1)) 1)"
        `shouldEqual` Right "2"

    it "quote" do
      Biwa.eval "(quote a)"
        `shouldEqual` Right "a"

      Biwa.eval "'a"
        `shouldEqual` Right "a"

      Biwa.eval "(quote quote)"
        `shouldEqual` Right "quote"

      -- Currently quotes are printed as `'` if applied to a single argument,
      -- as `quote` otherwise.
      Biwa.eval "(quote 'a)"
        `shouldEqual` Right "'a"

    it "eval" do
      Biwa.eval "(eval (quote 1))"
        `shouldEqual` Right "1"

    it "let" do
      Biwa.eval letExample
        `shouldEqual` Right "2"

    describe "list" do
      it "car" do
        Biwa.eval "(car '(a b))"
          `shouldEqual` Right "a"

      it "cdr" do
        Biwa.eval "(cdr '(a b))"
          `shouldEqual` Right "(b)"

      it "cons" do
        Biwa.eval "(cons 'a '(b c))"
          `shouldEqual` Right "(a b c)"

      it "list-empty?" do
        Biwa.eval "(list-empty? '())"
          `shouldEqual` Right "t"

        Biwa.eval "(list-empty? 'a)"
          `shouldEqual` Right "f"

      it "list-nonempty?" do
        Biwa.eval "(list-nonempty? '(a))"
          `shouldEqual` Right "t"

        Biwa.eval "(list-nonempty? '(a b))"
          `shouldEqual` Right "t"

        Biwa.eval "(list-nonempty? '(a b c))"
          `shouldEqual` Right "t"

        Biwa.eval "(list-nonempty? '())"
          `shouldEqual` Right "f"

        Biwa.eval "(list-nonempty? 'a)"
          `shouldEqual` Right "f"

    it "symbol?" do
      Biwa.eval "(symbol? 'a)"
        `shouldEqual` Right "t"

      Biwa.eval "(symbol? 1)"
        `shouldEqual` Right "f"

    it "symbol-eq?" do
      Biwa.eval "(symbol-eq? 'a 'a)"
        `shouldEqual` Right "t"

      Biwa.eval "(symbol-eq? 'a 'b)"
        `shouldEqual` Right "f"

      Biwa.eval "(symbol-eq? 'a 1)"
        `shouldEqual` Left "execute: unbound symbol: \"TODO-symbol?-eq-not-symbol\" [symbol-eq?]"

    it "if" do
      Biwa.eval "(if 't 'a 'b)"
        `shouldEqual` Right "a"

      Biwa.eval "(if 't 'a not-defined)"
        `shouldEqual` Right "a"

      Biwa.eval "(if (int? 1) 'a 'b)"
        `shouldEqual` Right "a"

      Biwa.eval "(if 'f 'a 'b)"
        `shouldEqual` Right "b"

      Biwa.eval "(if 'f not-defined 'b)"
        `shouldEqual` Right "b"

      Biwa.eval "(if 123 'a 'b)"
        `shouldEqual` Left "execute: unbound symbol: \"TODO-if-fallthrough\" [(anon)]"

    describe "number" do
      it "int?" do
        Biwa.eval "(int? 1)"
          `shouldEqual` Right "t"

        Biwa.eval "(int? 'a)"
          `shouldEqual` Right "f"

      it "+" do
        Biwa.eval "(+ 1 1)"
          `shouldEqual` Right "2"

      it "*" do
        Biwa.eval "(* 2 3)"
          `shouldEqual` Right "6"

      it "<" do
        Biwa.eval "(< 1 2)"
          `shouldEqual` Right "t"

        Biwa.eval "(< 2 1)"
          `shouldEqual` Right "f"

        Biwa.eval "(< 1 'a)"
          `shouldEqual` Left "number required, but got a-midnight [<-midnight-helper, <]"

        Biwa.eval "(= 1 1)"
          `shouldEqual` Right "t"

        Biwa.eval "(= 1 2)"
          `shouldEqual` Right "f"

        Biwa.eval "(= 1 'a)"
          `shouldEqual` Left "number required, but got a-midnight [=-midnight-helper, =]"

        Biwa.eval "(> 2 1)"
          `shouldEqual` Right "t"

        Biwa.eval "(> 1 2)"
          `shouldEqual` Right "f"

    describe "string literal" do
      it "works" do
        Biwa.eval "\"abc\""
          `shouldEqual` Right "(string-tag (97 98 99))"

    it "handles failure" do
      Biwa.eval "(+ 1 1"
        `shouldEqual`
          Left "(ParseError \"inside list: Expected ')'\" (Position { column: 7, index: 6, line: 1 }))"

letExample :: String
letExample =
  """
(let
  ((go (lambda (n) (+ n x)))
   (x 1))
  (go 1))
"""
