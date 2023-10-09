module MidnightJS.Transpile where

import Lib.Debug
import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import MidnightJS.Imp (Imp(..))
import MidnightJS.Imp as Imp
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
          Throw (JSString "Evaluated empty list")

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
      JSInt n

translateSymbol :: String -> String
translateSymbol sym =
  case Map.lookup sym Translate.midnightToJsMap of
    Nothing ->
      String.replaceAll (Pattern "-") (Replacement "_") sym <> "_midnight"

    Just jsName ->
      jsName

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
            ( JSString
                ( "Expected symbol or list of symbols for lambda expression, but got: "
                    <> show n
                )
            )

    params ->
      Throw
        ( JSString
            ( "Expected two arguments for lambda expression, but got: "
                <> show params
            )
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
            ( JSString
                ( "Expected symbol for lambda parameter, but got: "
                    <> e
                )
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
              Throw (JSString e)

            Right bindings ->
              Let bindings (transpile body)

        _ ->
          Throw
            ( JSString
                ( "Expected let bindings to be a list, but got: "
                    <> show bindingList
                )
            )

    params ->
      Throw
        ( JSString
            ( "Expected two arguments for let expression, but got: "
                <> show params
            )
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
        ( JSString
            ( "Expected three arguments for if expression, but got: "
                <> show params
            )
        )

quoteArgs :: PsList Sexp -> Imp
quoteArgs =
  case _ of
    param : List.Nil ->
      transpileQuote param

    params ->
      Throw
        ( JSString
            ( "Expected one argument for quote, but got: "
                <> show params
            )
        )

transpileQuote :: Sexp -> Imp
transpileQuote =
  case _ of
    Sexp.Symbol sym ->
      JSString sym

    Sexp.List xs ->
      Array (transpileQuote <$> xs)

    Sexp.Int n ->
      JSInt n
