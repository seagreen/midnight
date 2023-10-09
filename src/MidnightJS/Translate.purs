module MidnightJS.Translate where

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

midnightToJsMap :: Map String String
midnightToJsMap =
  Map.fromFoldable
    [ -- Classics (variables and `app` are implicit)
      Tuple "lambda" "lambda"

    -- Lispy
    , Tuple "quote" "quote" -- TODO: remove?
    , Tuple "eval" "evalMidnight"

    -- Conditional
    , Tuple "t" "t"
    , Tuple "f" "f"

    -- List
    , Tuple "car" "car"
    , Tuple "cdr" "cdr"
    , Tuple "cons" "cons"
    , Tuple "pair?" "isPair"
    , Tuple "list-empty?" "isListEmpty"

    -- Symbol
    , Tuple "symbol?" "isSymbol"
    , Tuple "symbol-eq?" "isSymbolEq"
    , Tuple "codepoints->symbol" "codepointsToSymbol"

    -- Int
    , Tuple "int?" "isInt"
    , Tuple "+" "add"
    , Tuple "-" "subtract"
    , Tuple "*" "multiply"
    , Tuple "/" "divide"
    , Tuple "%" "modulo"
    , Tuple "<" "lessThan"
    , Tuple "=" "equal"
    , Tuple ">" "greaterThan"

    -- Extra
    , Tuple "trace" "trace-midnight-helper"
    , Tuple "trace-bench" "trace-bench-midnight-helper"
    ]

jsToMidnightMap :: Map String String
jsToMidnightMap =
  Map.fromFoldable
    ( Tuple.swap
        <$> Map.toUnfoldable midnightToJsMap :: List (Tuple String String)
    )
