module MidnightJS where

import Debug
import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import MidnightJS.AST as AST
import MidnightJS.Foreign as Foreign
import MidnightJS.Imp as Imp
import MidnightJS.JsonToSexp (jsonToSexp)
import MidnightJS.TCE as TCE
import MidnightJS.Transpile as Transpile
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

midnightToJs :: Sexp -> String
midnightToJs midnight =
  AST.serialize
    ( Imp.toAST
        ( TCE.tailCallElimation
            (Transpile.transpile midnight)
        )
    )

eval :: String -> Either String String
eval midnightSrc =
  case Sexp.parse midnightSrc of
    Left e ->
      Left ("parse error: " <> e)

    Right midnightSexp ->
      case Foreign.evalToForeign (spy "js" (midnightToJs midnightSexp)) of
        Left e ->
          Left ("eval error: " <> e)

        Right jsResult ->
          case jsonToSexp jsResult of
            Left e ->
              Left ("jsonToSexp error: " <> e)

            Right resultSexp ->
              Right (Sexp.prettyprintColsPrefer80 resultSexp)

evalJsonToJson :: Json -> Either String Json
evalJsonToJson jsonVal =
  case jsonToSexp jsonVal of
    Left e ->
      Left ("parse jsonVal error: " <> e)

    Right midnightSexp ->
      case Foreign.evalToForeign (midnightToJs midnightSexp) of
        Left e ->
          Left ("eval error: " <> e)

        Right jsResult ->
          Right jsResult

evalToSexp :: String -> Either String Sexp
evalToSexp midnightSrc =
  case Sexp.parse midnightSrc of
    Left e ->
      Left ("parse error: " <> e)

    Right midnightSexp ->
      case Foreign.evalToForeign (spy "js" (midnightToJs midnightSexp)) of
        Left e ->
          Left ("eval error: " <> e)

        Right jsResult ->
          case jsonToSexp jsResult of
            Left e ->
              Left ("jsonToSexp error: " <> e)

            Right resultSexp ->
              Right resultSexp
