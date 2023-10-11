module MidnightJS
  ( eval
  , evalToSexp
  , evalJsonToJson
  , evalToForeign
  , applyClosure
  , midnightToJS
  ) where

import Prelude

import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Foreign (Foreign)
import MidnightJS.AST as AST
import MidnightJS.Foreign as Foreign
import MidnightJS.Imp as Imp
import MidnightJS.JsonToMidnightSexp (jsonToMidnightSexp)
import MidnightJS.TCE as TCE
import MidnightJS.Transpile as Transpile
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

eval :: String -> Either String String
eval midnightSrc = do
  midnightSexp <- parse midnightSrc

  jsResult <-
    lmap
      (\e -> "eval error: " <> e)
      (Foreign.evalToJson (midnightToJS midnightSexp))

  resultSexp <-
    lmap
      (\e -> "jsonToMidnightSexp error: " <> e)
      (jsonToMidnightSexp jsResult)

  pure (Sexp.prettyprintColsPrefer80 resultSexp)

evalToSexp :: String -> Either String Sexp
evalToSexp midnightSrc = do
  midnightSexp <- parse midnightSrc

  jsResult <-
    lmap
      (\e -> "eval error: " <> e)
      (Foreign.evalToJson (midnightToJS midnightSexp))

  lmap
    (\e -> "jsonToMidnightSexp error: " <> e)
    (jsonToMidnightSexp jsResult)

evalJsonToJson :: Json -> Either String Json
evalJsonToJson jsonVal = do
  sexpVal <-
    lmap
      (\e -> "jsonToMidnightSexp error: " <> e)
      (jsonToMidnightSexp jsonVal)

  lmap
    (\e -> "eval error: " <> e)
    (Foreign.evalToJson (midnightToJS sexpVal))

evalToForeign :: String -> Either String Foreign
evalToForeign midnightSrc = do
  midnightSexp <- parse midnightSrc

  lmap
    (\e -> "eval error: " <> e)
    (Foreign.evalToForeign (midnightToJS midnightSexp))

applyClosure :: Foreign -> Array Foreign -> Either String Foreign
applyClosure f args =
  Foreign.applyClosure f args

-- * Helpers

parse :: String -> Either String Sexp
parse midnightSrc =
  case Sexp.parse midnightSrc of
    Left e ->
      Left ("parse error: " <> e)

    Right midnightSexp ->
      Right midnightSexp

midnightToJS :: Sexp -> String
midnightToJS midnight =
  AST.serialize
    ( Imp.toAST
        ( TCE.tailCallElimation
            (Transpile.transpile midnight)
        )
    )
