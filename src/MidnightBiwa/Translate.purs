module MidnightBiwa.Translate where

import Prelude

import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Lib.Sexp as GenericSexp
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

midnightToBiwaMap :: Map String String
midnightToBiwaMap =
  Map.fromFoldable
    [ -- Classics (variables and `app` are implicit)
      Tuple "lambda" "lambda"
    , Tuple "let" "letrec*"

    -- Lispy
    , Tuple "quote" "quote"
    , Tuple "eval" "eval-midnight-helper"

    -- Conditional
    , Tuple "if" "if-midnight-helper"
    , Tuple "t" "t"
    , Tuple "f" "f"

    -- List
    , Tuple "car" "car"
    , Tuple "cdr" "cdr"
    , Tuple "cons" "cons"
    , Tuple "list-empty?" "list-empty?"
    , Tuple "list-nonempty?" "list-nonempty?"

    -- Symbol
    , Tuple "symbol?" "symbol?-midnight-helper"
    , Tuple "symbol-eq?" "symbol-eq?"
    , Tuple "codepoints-to-symbol" "codepoints-to-symbol-midnight-helper"

    -- Int
    , Tuple "int?" "int?"
    , Tuple "+" "+"
    , Tuple "-" "-"
    , Tuple "*" "*"
    , Tuple "/" "/-midnight-helper"
    , Tuple "<" "<-midnight-helper"
    , Tuple "=" "=-midnight-helper"
    , Tuple ">" ">-midnight-helper"

    -- Extra
    , Tuple "trace" "trace-midnight-helper"
    , Tuple "trace-bench" "trace-bench-midnight-helper"
    ]

biwaToMidnightMap :: Map String String
biwaToMidnightMap =
  Map.fromFoldable
    ( Tuple.swap
        <$> Map.toUnfoldable midnightToBiwaMap :: List (Tuple String String)
    )

-- * Midnight to Biwa

midnightToBiwaPlusSnippet :: String -> Either String String
midnightToBiwaPlusSnippet midnightSrc = do
  sexp <- Sexp.parse midnightSrc
  pure (midnightSexpToBiwaPlusSnippet sexp)

midnightSexpToBiwaPlusSnippet :: Sexp -> String
midnightSexpToBiwaPlusSnippet midnightSexp =
  extraBiwaCode
    <> "\n"
    <> midnightSexpToBiwaNoSnippet midnightSexp

midnightSexpToBiwaNoSnippet :: Sexp -> String
midnightSexpToBiwaNoSnippet midnightSexp =
  GenericSexp.print (toBiwa midnightSexp)
  where
  toBiwa :: Sexp -> GenericSexp.Sexp
  toBiwa = case _ of
    Sexp.Symbol s ->
      GenericSexp.Atom
        ( case Map.lookup s midnightToBiwaMap of
            Just biwa ->
              biwa

            Nothing ->
              s <> "-midnight"
        )

    Sexp.List xs ->
      GenericSexp.List (toBiwa <$> xs)

    Sexp.Int n ->
      GenericSexp.Atom (show n)

-- * Biwa to Midnight

biwaToMidnight :: String -> Either String String
biwaToMidnight biwaSrc = do
  biwaSexp <- GenericSexp.parse biwaSrc
  Sexp.print <$> biwaSexpToMidnightSexp biwaSexp

biwaToMidnightSexp :: String -> Either String Sexp
biwaToMidnightSexp biwaSrc = do
  biwaSexp <- GenericSexp.parse biwaSrc
  biwaSexpToMidnightSexpWithMap biwaToMidnightMap biwaSexp

biwaSexpToMidnightSexp :: GenericSexp.Sexp -> Either String Sexp
biwaSexpToMidnightSexp sexp =
  biwaSexpToMidnightSexpWithMap mapWithClosure sexp
  where
  mapWithClosure :: Map String String
  mapWithClosure =
    Map.insert "#<Closure>" "#<Closure>" biwaToMidnightMap

biwaSexpToMidnightSexpWithMap :: Map String String -> GenericSexp.Sexp -> Either String Sexp
biwaSexpToMidnightSexpWithMap biwaToMidnightMapArg =
  case _ of
    GenericSexp.Atom a ->
      case Map.lookup a biwaToMidnightMapArg of
        Just s ->
          pure (Sexp.Symbol s)

        Nothing ->
          case Sexp.parseInt a of
            Right n ->
              pure (Sexp.Int n)

            Left _ ->
              case String.stripSuffix (String.Pattern "-midnight") a of
                Nothing ->
                  Left ("biwaSexpToMidnightSexp stripSuffix failed for: " <> a)

                Just s ->
                  pure (Sexp.Symbol s)

    GenericSexp.List xs ->
      Sexp.List <$> (for xs biwaSexpToMidnightSexp)

    GenericSexp.StringLiteral _ ->
      Left "biwaSexpToMidnightSexp: unexpected string literal"

-- * Biwa translation table

biwaTranslationAlistCode :: String
biwaTranslationAlistCode =
  """(define translation-alist-midnight-internal
""" <> toBiwaCodeAlist midnightToBiwaMap
    <> ")"

toBiwaCodeAlist :: Map String String -> String
toBiwaCodeAlist dict =
  "'("
    <>
      ( foldMapWithIndex
          (\k v -> "(" <> doublequote k <> " " <> doublequote v <> ")")
          dict
          <> ")"
      )
  where
  doublequote a = "\"" <> a <> "\""

-- * Biwa Snippet

extraBiwaCode :: String
extraBiwaCode =
  biwaTranslationAlistCode <>
    """
(define-macro (if-midnight-helper if-b then-branch else-branch)
  `(let
    ((b-val ,if-b))
    (if
    (eqv? b-val 't)
    ,then-branch
    (if
      (eqv? b-val 'f)
      ,else-branch
      TODO-if-fallthrough))))

(define (internal-biwa-to-midnight-bool b)
  (if
    (eqv? b #t)
    't
    (if
      (eqv? b #f)
      'f
      TODO-if-internal-biwa-to-midnight-fallthrough)))

(define (eval-midnight-helper eval-arg)
  (let
    ((res (eval eval-arg)))
    (if
      (js-undefined? res)
      (TODO-eval-undefined)
      res)))

(define (list-empty? a)
  (internal-biwa-to-midnight-bool (null? a)))

(define (list-nonempty? a)
  (internal-biwa-to-midnight-bool (pair? a)))

(define (symbol?-midnight-helper a)
  (internal-biwa-to-midnight-bool (symbol? a)))

(define (symbol-eq? a b)
  (if
    (and (symbol? a) (symbol? b))
    (internal-biwa-to-midnight-bool (eqv? a b))
    (TODO-symbol?-eq-not-symbol)))

(define (codepoints-to-symbol-midnight-helper codepoints)
  (let*
    ((str (list->string (map integer->char codepoints)))
     (res (assoc str translation-alist-midnight-internal)))
    (if
      res
      (string->symbol (cadr res))
      (string->symbol (string-append str "-midnight")))))

(define (int? a)
  (internal-biwa-to-midnight-bool (number? a)))

(define (/-midnight-helper divident divisor)
  (let
    ((quotient (quotient divident divisor)))
    (if
      (finite? quotient)
      quotient
      ('crash-quotient-not-finite))))

(define (<-midnight-helper a b)
  (internal-biwa-to-midnight-bool (< a b)))

(define (=-midnight-helper a b)
  (internal-biwa-to-midnight-bool (= a b)))

(define (>-midnight-helper a b)
  (internal-biwa-to-midnight-bool (> a b)))

(define (trace-midnight-helper a)
  (begin
    (display a)
    (display "\n")
    a))

(define (trace-bench-midnight-helper label a)
  (let
    ((start-date (js-eval "performance.now()"))
     (res (a '()))
     (finish-date (js-eval "performance.now()")))
    (begin
      ; (display res)
      (display label)
      (display "\n")
      (display (- finish-date start-date))
      (display "ms")
      (display "\n")
      res)))
"""
