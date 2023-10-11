module MidnightJS.Transpile where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (foldr)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Lib.Debug (crash)
import MidnightJS.Imp (Imp(..))
import MidnightJS.Translate as Translate
import MidnightLang.Sexp (PsList, Sexp)
import MidnightLang.Sexp as Sexp

transpile :: Sexp -> Imp
transpile =
  case _ of
    Sexp.Symbol sym ->
      Var (translateSymbol sym)

    Sexp.List xs ->
      case xs of
        List.Nil ->
          Throw "Evaluated empty list"

        List.Cons x rest ->
          case x of
            Sexp.Symbol "lambda" ->
              transpileLambda rest

            Sexp.Symbol "let" ->
              transpileLet rest

            Sexp.Symbol "quote" ->
              quoteArgs rest

            Sexp.Symbol "if" ->
              transpileIf rest

            _ ->
              App (transpile x) (transpile <$> rest)

    Sexp.Int n ->
      Int n

translateSymbol :: String -> String
translateSymbol sym =
  case Map.lookup sym Translate.midnightToJsMap of
    Nothing ->
      sanitize sym <> "_midnight"

    Just jsName ->
      jsName
  where
  sanitize :: String -> String
  sanitize =
    Array.foldMap sanitizeChar
      <<< sanitizeFirstChar
      <<< String.toCodePointArray
      <<< String.replaceAll (Pattern "?") (Replacement "_q")
      <<< String.replaceAll (Pattern "-") (Replacement "_")

  sanitizeChar :: CodePoint -> String
  sanitizeChar c =
    if isAllowedChar c then String.singleton c
    else replaceChar c

  replaceChar :: CodePoint -> String
  replaceChar c =
    "$codepoint_" <> show (fromEnum c)

  sanitizeFirstChar :: Array CodePoint -> Array CodePoint
  sanitizeFirstChar xs =
    case Array.uncons xs of
      Just { head, tail } ->
        if isAllowedFirstChar head then
          xs
        else
          String.toCodePointArray (replaceChar head) <> tail

      Nothing ->
        crash "sanitizeFirstChar hit empty identifier"

  isAllowedChar :: CodePoint -> Boolean
  isAllowedChar c =
    isAllowedFirstChar c
      || (c >= codePointFromChar '0' && c <= codePointFromChar '1')

  isAllowedFirstChar :: CodePoint -> Boolean
  isAllowedFirstChar c =
    (c >= codePointFromChar 'a' && c <= codePointFromChar 'z')
      || (c >= codePointFromChar 'A' && c <= codePointFromChar 'Z')
      || c == codePointFromChar '_'

transpileLambda :: PsList Sexp -> Imp
transpileLambda =
  case _ of
    params : body : List.Nil ->
      case params of
        Sexp.Symbol sym ->
          -- NOTE: note the translateSymbols
          LamVariadic (translateSymbol sym) (transpile body)

        Sexp.List xs ->
          toStringParams
            xs
            ( \stringParams ->
                Lam (translateSymbol <$> stringParams) (transpile body)
            )

        Sexp.Int n ->
          Throw
            ( "Expected symbol or list of symbols for lambda expression, but got: "
                <> show n
            )

    params ->
      Throw
        ( "Expected two arguments for lambda expression, but got: "
            <> show params
        )
  where
  toStringParams :: PsList Sexp -> (PsList String -> Imp) -> Imp
  toStringParams params f =
    let
      toString :: Sexp -> Either String String
      toString =
        case _ of
          Sexp.Symbol sym ->
            Right sym

          Sexp.List xs ->
            Left (show xs)

          Sexp.Int n ->
            Left (show n)
    in
      case for params toString of
        Left e ->
          Throw
            ( "Expected symbol for lambda parameter, but got: "
                <> e
            )

        Right stringParams ->
          f stringParams

transpileLet :: PsList Sexp -> Imp
transpileLet =
  case _ of
    bindingList : body : List.Nil ->
      case bindingList of
        Sexp.List bindingSexps ->
          case transpileBindings bindingSexps of
            Left e ->
              Throw e

            Right bindings ->
              Let bindings (transpile body)

        _ ->
          Throw
            ( "Expected let bindings to be a list, but got: "
                <> show bindingList
            )

    params ->
      Throw
        ( "Expected two arguments for let expression, but got: "
            <> show params
        )
  where
  transpileBindings :: PsList Sexp -> Either String (PsList (Tuple String Imp))
  transpileBindings bindingSexps =
    let
      toBinding :: Sexp -> Either String (Tuple String Imp)
      toBinding =
        case _ of
          Sexp.List (Sexp.Symbol name : val : List.Nil) ->
            -- NOTE the translateSymbol:
            Right (Tuple (translateSymbol name) (transpile val))

          other ->
            Left ("Expected let binding list entry, got: " <> show other)

    in
      for bindingSexps toBinding

transpileIf :: PsList Sexp -> Imp
transpileIf =
  case _ of
    predicate : consequent : alternative : List.Nil ->
      If (transpile predicate) (transpile consequent) (transpile alternative)

    params ->
      Throw
        ( "Expected three arguments for if expression, but got: "
            <> show params
        )

quoteArgs :: PsList Sexp -> Imp
quoteArgs =
  case _ of
    param : List.Nil ->
      transpileQuote param

    params ->
      Throw
        ( "Expected one argument for quote, but got: "
            <> show params
        )

transpileQuote :: Sexp -> Imp
transpileQuote =
  case _ of
    Sexp.Symbol sym ->
      -- NOTE(QUOTED_SYMBOLS_NOT_SANITIZED)
      --
      -- When sent through an eval they get become variables
      -- and get sanitized, but until then they're not.
      --
      -- For example, when used in a comparison:
      -- ```
      -- symbol-eq? 'input-normal input
      -- ```
      --
      -- At first I thought the outside system would need to pass in
      -- `input_normal_midnight` for the input variable,
      -- but this isn't correct.
      ImpString sym

    Sexp.List xs ->
      foldr
        (\x acc -> Pair (transpileQuote x) acc)
        NilList
        xs
    -- Array (transpileQuote <$> xs)
    {-
    case xs of
      List.Nil ->
        Array List.Nil

      List.Cons x rest ->
        Array
          ( transpileQuote x
              : transpileQuote (Sexp.List rest)
              : List.Nil
          )
    -}

    Sexp.Int n ->
      Int n

{-
pairize :: Sexp -> Sexp
pairize =
  case _ of
    Sexp.Symbol sym ->
      Sexp.Symbol sym

    Sexp.List xs ->
      convertToConsList xs

    Sexp.Int n ->
      Sexp.Int n

convertToConsList :: PsList Sexp -> Sexp
convertToConsList xs =
  case xs of
    List.Nil ->
      Sexp.List List.Nil

    List.Cons x rest ->
      -- TODO: perf   
      Sexp.List (pairize x : convertToConsList rest : List.Nil)
-}