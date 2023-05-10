module Test.Lib.Sexp where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.List as PsList
import Data.String.CodeUnits (fromCharArray)
import Lib.Sexp (Sexp(..))
import Lib.Sexp as Sexp
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

list :: Array Sexp -> Sexp
list =
  List <<< PsList.fromFoldable

spec :: Spec Unit
spec = do
  describe "print" do
    it "atom" do
      Sexp.print (Atom "abc")
        `shouldEqual` "abc"

      Sexp.print (Atom "ab(c")
        `shouldEqual` "ab(c"

    it "string" do
      Sexp.print (StringLiteral "abc")
        `shouldEqual` (charstr [ '"', 'a', 'b', 'c', '"' ])

      Sexp.print (StringLiteral (charstr [ 'a', 'b', '"', 'c' ]))
        `shouldEqual` (charstr [ '"', 'a', 'b', '\\', '"', 'c', '"' ])

  describe "parse" do
    it "atom" do
      Sexp.parse "abc" `shouldEqual` Right (Atom "abc")
      Sexp.parse "abc " `shouldEqual` Right (Atom "abc")
      Sexp.parse "abc (" `shouldSatisfy` isLeft
      Sexp.parse """ab\(""" `shouldSatisfy` isLeft
      Sexp.parse """ab\c""" `shouldSatisfy` isLeft
      Sexp.parse """ab\\c""" `shouldSatisfy` isLeft
      Sexp.parse "ab\"c" `shouldSatisfy` isLeft
      Sexp.parse "ab\\\"c" `shouldSatisfy` isLeft

    it "parenthesized-expression" do
      Sexp.parse "()" `shouldEqual`
        Right (list [])
      Sexp.parse "(abc)" `shouldEqual`
        Right (list [ Atom "abc" ])
      Sexp.parse "(aaa bbb)" `shouldEqual`
        Right (list [ Atom "aaa", Atom "bbb" ])
      Sexp.parse "(aaa bbb\\))" `shouldSatisfy` isLeft
      Sexp.parse "(aaa (bbb ccc))" `shouldEqual`
        Right
          ( list
              [ Atom "aaa"
              , list
                  [ Atom "bbb"
                  , Atom "ccc"
                  ]
              ]
          )
      Sexp.parse " ( aaa ( bbb ccc ) ) " `shouldEqual`
        Right
          ( list
              [ Atom "aaa"
              , list
                  [ Atom "bbb"
                  , Atom "ccc"
                  ]
              ]
          )
      Sexp.parse "(abc))" `shouldSatisfy` isLeft

    it "quote" do
      Sexp.parse "'a" `shouldEqual`
        Right (list [ Atom "quote", Atom "a" ])
      Sexp.parse "''a" `shouldEqual`
        Right (list [ Atom "quote", list [ Atom "quote", Atom "a" ] ])
      Sexp.parse "'()" `shouldEqual`
        Right (list [ Atom "quote", list [] ])
      Sexp.parse "''()" `shouldEqual`
        Right (list [ Atom "quote", list [ Atom "quote", list [] ] ])
      Sexp.parse "'(a b)" `shouldEqual`
        Right (list [ Atom "quote", list [ Atom "a", Atom "b" ] ])

    it "comment" do
      Sexp.parse "1 ; a" `shouldEqual`
        Right (Atom "1")

      Sexp.parse "1 ; a\n" `shouldEqual`
        Right (Atom "1")

      Sexp.parse "1 ; a\n\n" `shouldEqual`
        Right (Atom "1")

      Sexp.parse "(1) ; a" `shouldEqual`
        Right (list [ Atom "1" ])

      Sexp.parse lotsOfComments `shouldEqual`
        Right (list [ Atom "1", Atom "2" ])

    it "string literal" do
      Sexp.parse "\"\"" `shouldEqual`
        Right (StringLiteral "")
      Sexp.parse "\"abc\"" `shouldEqual`
        Right (StringLiteral "abc")
      Sexp.parse "\"ab  c\"" `shouldEqual`
        Right (StringLiteral "ab  c")
      Sexp.parse "\"a\\\"bc\"" `shouldEqual`
        Right (StringLiteral "a\"bc")
      Sexp.parse "\"ab\\(c\"" `shouldSatisfy` isLeft
      Sexp.parse "\"ab\\\\c\"" `shouldEqual`
        Right (StringLiteral "ab\\c")

    it "newline literal in string" do
      Sexp.parse "\"nnn\"" `shouldEqual`
        Right (StringLiteral "nnn")

      -- Test parsing of backslash+n to newline
      Sexp.parse (charstr [ '"', 'a', 'b', '\\', 'n', 'c', '"' ]) `shouldEqual`
        Right (StringLiteral (charstr [ 'a', 'b', '\n', 'c' ]))

    it "regression" do
      Sexp.parse "(() a)" `shouldEqual`
        Right (list [ list [], Atom "a" ])

lotsOfComments :: String
lotsOfComments =
  """; aaa
; bbb

(1 ; ccc

; eee
;ddd

;; fff ; ggg

2 ; hhh)))
)

; iii
"""

-- | Using single quotes for strings means they get double escaped,
-- | once for PureScript string literal syntax and once for the sexpr's.
-- |
-- | I'd like to use PureScript triple quotes for everything `"""`,
-- | but unfortunately that messes up my editor's syntax highlighting,
-- | so using this instead.
charstr :: Array Char -> String
charstr =
  fromCharArray
