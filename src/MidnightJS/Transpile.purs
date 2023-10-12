module MidnightJS.Transpile where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Control.Monad.Trampoline (Trampoline, delay, done, runTrampoline)
import Control.Monad.Trans.Class (lift)
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
  runTrampoline <<< transpileGo

transpileGo :: Sexp -> Trampoline Imp
transpileGo =
  case _ of
    Sexp.Symbol sym ->
      done (Var (translateSymbol sym))

    Sexp.List xs ->
      case xs of
        List.Nil ->
          done (Throw "Evaluated empty list")

        List.Cons x rest ->
          case x of
            Sexp.Symbol "lambda" ->
              transpileLambda rest

            Sexp.Symbol "let" ->
              transpileLet rest

            Sexp.Symbol "quote" ->
              done (quoteArgs rest)

            Sexp.Symbol "if" ->
              transpileIf rest

            _ -> do
              xImp <- transpileGo x
              restImp <- for rest transpileGo
              done (App xImp restImp)

    Sexp.Int n ->
      done (Int n)

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

transpileLambda :: PsList Sexp -> Trampoline Imp
transpileLambda =
  case _ of
    params : body : List.Nil ->
      case params of
        Sexp.Symbol sym -> do
          bodyImp <- transpileGo body
          -- NOTE: note the translateSymbols
          done (LamVariadic (translateSymbol sym) bodyImp)

        Sexp.List xs ->
          case toStringParams xs of
            Left e ->
              done
                ( Throw
                    ( "Expected symbol for lambda parameter, but got: "
                        <> e
                    )
                )

            Right stringParams -> do
              bodyImp <- transpileGo body
              done (Lam (translateSymbol <$> stringParams) bodyImp)

        Sexp.Int n ->
          done
            ( Throw
                ( "Expected symbol or list of symbols for lambda expression, but got: "
                    <> show n
                )
            )

    params ->
      done
        ( Throw
            ( "Expected two arguments for lambda expression, but got: "
                <> show params
            )
        )
  where
  toStringParams :: PsList Sexp -> Either String (PsList String)
  toStringParams params =
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
      for params toString

transpileLet :: PsList Sexp -> Trampoline Imp
transpileLet =
  case _ of
    bindingList : body : List.Nil ->
      case bindingList of
        Sexp.List bindingSexps -> do
          eBindingsImp <- runExceptT (transpileBindings bindingSexps)
          case eBindingsImp of
            Left e ->
              done (Throw e)

            Right bindings -> do
              bodyImp <- transpileGo body
              done (Let bindings bodyImp)

        _ ->
          done
            ( Throw
                ( "Expected let bindings to be a list, but got: "
                    <> show bindingList
                )
            )

    params ->
      done
        ( Throw
            ( "Expected two arguments for let expression, but got: "
                <> show params
            )
        )
  where
  transpileBindings :: PsList Sexp -> ExceptT String Trampoline (PsList (Tuple String Imp))
  transpileBindings bindingSexps =
    let
      toBinding :: Sexp -> ExceptT String Trampoline (Tuple String Imp)
      toBinding =
        case _ of
          Sexp.List (Sexp.Symbol name : val : List.Nil) -> do
            valImp <- lift (transpileGo val)
            -- NOTE the translateSymbol:
            pure (Tuple (translateSymbol name) valImp)

          other ->
            except (Left ("Expected let binding list entry, got: " <> show other))

    in
      for bindingSexps toBinding

transpileIf :: PsList Sexp -> Trampoline Imp
transpileIf =
  case _ of
    predicate : consequent : alternative : List.Nil -> do
      predicateImp <- transpileGo predicate
      consequentImp <- transpileGo consequent
      alternativeImp <- transpileGo alternative
      done (If predicateImp consequentImp alternativeImp)

    params ->
      done
        ( Throw
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
        ( "Expected one argument for quote, but got: "
            <> show params
        )

transpileQuote :: Sexp -> Imp
transpileQuote =
  runTrampoline <<< go
  where
  -- Example Trampoline use:
  -- https://github.com/pure-c/purec/blob/1b7ff43cc011efd4ebb44cfa389e16a7ca954c60/src/Language/PureScript/CodeGen/C/AST.purs#L257
  go :: Sexp -> Trampoline Imp
  go =
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
        done (ImpString sym)

      Sexp.List xs ->
        foldrM
          ( \x acc -> do
              xImp <- go x
              done (Pair xImp acc)
          )
          NilList
          xs

      Sexp.Int n ->
        done (Int n)

  -- TODO: hmm https://discourse.purescript.org/t/where-is-monadic-fold-right/3701
  foldrM f z =
    -- foldr's stack safe for Lists:
    -- https://github.com/purescript/purescript-lists/blob/26e03e387c027ba10a65524e1b7b9c958dee1c2f/src/Data/List/Types.purs#L102
    foldr
      ( \x acc ->
          f x =<< acc
      )
      (pure z)
