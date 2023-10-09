module MidnightJS where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import MidnightJS.Foreign as Foreign
import MidnightJS.Imp as Imp
import MidnightJS.JsonToSexp (jsonToSexp)
import MidnightJS.Transpile (transpile)
import MidnightLang.Sexp as Sexp

eval :: String -> Either String String
eval midnightSrc =
  case Sexp.parse midnightSrc of
    Left e ->
      Left ("parse error: " <> e)

    Right midnightSexp ->
      case Foreign.evalToForeign (Imp.serialize (transpile midnightSexp)) of
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
      case Foreign.evalToForeign (Imp.serialize (transpile midnightSexp)) of
        Left e ->
          Left ("eval error: " <> e)

        Right jsResult ->
          Right jsResult
