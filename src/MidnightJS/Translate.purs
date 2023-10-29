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
    [ -- Lambda calculus
      --
      -- (variables, application, and `let` don't translate
      -- to JS identifiers)
      --
      -- TODO: should lambda be removed from here as well?
      -- Not sure what semantics we want for:
      --
      -- ```
      -- ((lambda (lambda) lambda) 1)
      -- ```
      Tuple "lambda" "lambda"

    -- Lispy (`quote` doesn't translate to a JS identifier)
    , Tuple "eval" "evalMidnight"

    -- Conditional (`if` doesn't translate to a JS identifier)
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
    , Tuple "symbol->codepoints" "symbolToCodepoints"

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
    , Tuple "crash" "crashMidnightHelper"
    , Tuple "trace" "traceMidnightHelper"
    , Tuple "trace-bench" "tracebenchMidnightHelper"
    ]

jsToMidnightMap :: Map String String
jsToMidnightMap =
  Map.fromFoldable
    ( Tuple.swap
        <$> Map.toUnfoldable midnightToJsMap :: List (Tuple String String)
    )
