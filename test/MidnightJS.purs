module Test.MidnightJS where

import Prelude

import Data.Either (Either(..))
import MidnightJS as JS
import MidnightJS as MidnightJS
import MidnightJS.Transpile as Transpile
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "midnight evaluated through JS" do
    describe "integer" do
      it "1" do
        JS.eval "1"
          `shouldEqual` Right "1"

      it "int?" do
        JS.eval "(int? 1)"
          `shouldEqual` Right "t"

        -- also introduce `quote` and `symbol`:
        JS.eval "(int? 'a)"
          `shouldEqual` Right "f"

      it "+" do
        JS.eval "(+ 1 2)"
          `shouldEqual` Right "3"

      it "-" do
        JS.eval "(- 3 2)"
          `shouldEqual` Right "1"

      it "*" do
        JS.eval "(* 2 3)"
          `shouldEqual` Right "6"

      it "/" do
        JS.eval "(/ 6 3)"
          `shouldEqual` Right "2"

        JS.eval "(/ 7 3)"
          `shouldEqual` Right "2"

      it "%" do
        JS.eval "(% 2 1)"
          `shouldEqual` Right "0"

        JS.eval "(% 3 2)"
          `shouldEqual` Right "1"

      it "<" do
        JS.eval "(< 1 2)"
          `shouldEqual` Right "t"

        JS.eval "(< 2 1)"
          `shouldEqual` Right "f"

      -- JS.eval "(< 1 'a)"
      --   `shouldEqual` Left "number required, but got a-midnight [<-midnight-helper, <]"

      it "=" do
        JS.eval "(= 1 1)"
          `shouldEqual` Right "t"

        JS.eval "(= 1 2)"
          `shouldEqual` Right "f"

      -- JS.eval "(= 1 'a)"
      --   `shouldEqual` Left "number required, but got a-midnight [=-midnight-helper, =]"

      it ">" do
        JS.eval "(> 2 1)"
          `shouldEqual` Right "t"

        JS.eval "(> 1 2)"
          `shouldEqual` Right "f"

    describe "symbol" do
      it "'a" do
        JS.eval "'a"
          `shouldEqual` Right "a"

      it "symbol?" do
        JS.eval "(symbol? 'a)"
          `shouldEqual` Right "t"

        JS.eval "(symbol? 1)"
          `shouldEqual` Right "f"

      it "symbol-eq?" do
        JS.eval "(symbol-eq? 'a 'a)"
          `shouldEqual` Right "t"

        JS.eval "(symbol-eq? 'a 'b)"
          `shouldEqual` Right "f"

      -- JS.eval "(symbol-eq? 'a 1)"
      --     `shouldEqual` Left "execute: unbound symbol: \"TODO-symbol?-eq-not-symbol\" [symbol-eq?]"

      it "codepoints->symbol" do
        JS.eval "(codepoints->symbol '(97 98 99))"
          `shouldEqual` Right "abc"

    describe "list" do
      it "'()" do
        JS.eval "'()"
          `shouldEqual` Right "()"

        JS.eval "'(a b)"
          `shouldEqual` Right "(a b)"

      it "car" do
        JS.eval "(car '(a b))"
          `shouldEqual` Right "a"

      it "cdr" do
        JS.eval "(cdr '(a b))"
          `shouldEqual` Right "(b)"

      it "cons" do
        JS.eval "(cons 'a '())"
          `shouldEqual` Right "(a)"

        JS.eval "(cons 'a '(b))"
          `shouldEqual` Right "(a b)"

        JS.eval "(cons 'a '(b c))"
          `shouldEqual` Right "(a b c)"

      it "pair?" do
        JS.eval "(pair? '(a))"
          `shouldEqual` Right "t"

        JS.eval "(pair? '(a b))"
          `shouldEqual` Right "t"

        JS.eval "(pair? '(a b c))"
          `shouldEqual` Right "t"

        JS.eval "(pair? '())"
          `shouldEqual` Right "f"

        JS.eval "(pair? 'a)"
          `shouldEqual` Right "f"

      it "list-empty?" do
        JS.eval "(list-empty? '())"
          `shouldEqual` Right "t"

        JS.eval "(list-empty? '(a))"
          `shouldEqual` Right "f"

        JS.eval "(list-empty? 'a)"
          `shouldEqual` Right "f"

    describe "quote" do
      it "works" do
        JS.eval "(quote a)"
          `shouldEqual` Right "a"

        JS.eval "(quote (quote a))"
          -- TODO: should be coming out as 'a
          `shouldEqual` Right "(quote a)"

        JS.eval "''a"
          `shouldEqual` Right "(quote a)"

        -- Currently quotes are printed as `'` if applied to a single argument,
        -- as `quote` otherwise.
        -- Not working right now, see above TODO
        JS.eval "'(quote quote quote)"
          `shouldEqual` Right "(quote quote quote)"

    describe "if" do
      it "works" do
        JS.eval "(if 't 'a 'b)"
          `shouldEqual` Right "a"

        JS.eval "(if 't 'a not-defined)"
          `shouldEqual` Right "a"

        JS.eval "(if (int? 1) 'a 'b)"
          `shouldEqual` Right "a"

        JS.eval "(if 'f 'a 'b)"
          `shouldEqual` Right "b"

        JS.eval "(if 'f not-defined 'b)"
          `shouldEqual` Right "b"

    describe "eval" do
      it "(eval 1)" do
        JS.eval "(eval 1)"
          `shouldEqual` Right "1"

      it "(eval ''a)" do
        JS.eval "(eval ''a)"
          `shouldEqual` Right "a"

      it "(eval ''(a b))" do
        JS.eval "(eval ''(a b))"
          `shouldEqual` Right "(a b)"

    --JS.eval "(if 123 'a 'b)"
    --     `shouldEqual` Left "execute: unbound symbol: \"TODO-if-fallthrough\" [(anon)]"

    describe "lambda" do
      it "lambda-fixed-args" do
        JS.eval "((lambda (n) (+ n 1)) 2)"
          `shouldEqual` Right "3"

      it "lambda-variadic" do
        JS.eval "((lambda xs (cons 'a xs)) 'b 'c)"
          `shouldEqual` Right "(a b c)"

    describe "let" do
      it "works" do
        JS.eval letExample
          `shouldEqual` Right "2"

      it "allows-recursion" do
        JS.eval letRecursiveExample
          `shouldEqual` Right "a"

      it "allows-recursion-for-varargs-functions" do
        JS.eval letRecursiveExampleVarArgs
          `shouldEqual` Right "a"

      it "recursive let can be nested in another recursive let" do
        JS.eval nestedLetRecursiveExample
          `shouldEqual` Right "a"

    describe "string literal" do
      it "works" do
        JS.eval "\"abc\""
          `shouldEqual` Right "(string-tag (97 98 99))"

    -- Done testing specific functionality,
    -- now test interactions.

    describe "test-more-complex" do
      it "works" do
        JS.eval "(eval '(int? 1))"
          `shouldEqual` Right "t"

letExample :: String
letExample =
  """
(let
  ((go (lambda (n) (+ n x)))
   (x 1))
  (go 1))
"""

letRecursiveExample :: String
letRecursiveExample =
  """
(let
  ((go
    (lambda (n)
      (if
        (= n 0)
        'a
        (go (- n 1))))))
  (go 2))
"""

letRecursiveExampleVarArgs :: String
letRecursiveExampleVarArgs =
  """
(let
  ((go
    (lambda n
      (if
        (= (car n) 0)
        'a
        (go (- (car n) 1))))))
  (go 20000)) ; TODO: lower when done
"""

nestedLetRecursiveExample :: String
nestedLetRecursiveExample =
  """
(let
  ((go
    (let
      ((nested-go
        (lambda (x)
          (if
            (= x 0)
            'a
            (nested-go (- x 1))))))
      (lambda (n)
        (if
          (= n 0)
          (nested-go 2)
          (go (- n 1)))))))
  (go 2))
"""
