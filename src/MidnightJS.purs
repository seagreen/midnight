module MidnightJS
  ( eval
  , evalToSexp
  , evalJsonToJson
  , evalJsonToForeignNoCatch
  , evalToForeign
  , applyClosure
  , midnightToJS
  , jsToMidnight
  , parse
  , evalDebug
  ) where

import Prelude

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Argonaut (Json)
import Data.Argonaut (stringify)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Foreign (Foreign)
import Lib.Debug (crash)
import MidnightJS.AST as AST
import MidnightJS.Foreign as Foreign
import MidnightJS.Imp as Imp
import MidnightJS.JsonToMidnightSexp (flattenSexpLists, jsonToMidnightSexp, jsonToMidnightSexpNoFlatten)
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

evalJsonToForeignNoCatch :: Json -> Foreign
evalJsonToForeignNoCatch jsonVal = do
  case jsonToMidnightSexp jsonVal of
    Left e ->
      crash ("jsonToMidnightSexp error: " <> e)

    Right sexpVal ->
      Foreign.evalToForeignNoCatch (midnightToJS sexpVal)

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
midnightToJS midnightSexp =
  AST.serialize
    ( Imp.toAST
        ( TCE.tailCallElimation
            (Transpile.transpile midnightSexp)
        )
    )

jsToMidnight :: Json -> Either String String
jsToMidnight json =
  Sexp.prettyprintColsPrefer80 <$> jsonToMidnightSexp json

-- | Doesn't give an actual result, instead returns a string
-- describing what it's done.
evalDebug :: String -> String
evalDebug midnightSrc =
  Array.intercalate "\n\n" (execWriter go)
  where
  go :: Writer (Array String) Unit
  go = do
    tell [ "-------------------------------------------" ]
    case Sexp.parse midnightSrc of
      Left e ->
        tell [ "parse error:\n" <> e ]

      Right midnightSexp -> do
        tell [ "parse:\n" <> show midnightSexp ]
        tell [ "parse (prettyprinted)):\n" <> Sexp.prettyprintColsPrefer80 midnightSexp ]

        let transpiled = Transpile.transpile midnightSexp
        tell [ "transpile:\n" <> show transpiled ]

        let tce = TCE.tailCallElimation transpiled
        tell [ "tce:\n" <> show tce ]

        let imp = Imp.toAST tce
        tell [ "toAST:\n" <> show imp ]

        let js = AST.serialize imp
        tell [ "js:\n" <> js ]

        case Foreign.evalToJson js of
          Left e ->
            tell [ "eval error:\n" <> e ]

          Right resultJson -> do
            tell [ "eval result:\n" <> stringify resultJson ]

            case jsonToMidnightSexpNoFlatten resultJson of
              Left e ->
                tell [ "jsonToMidnightSexpNoFlatten error:\n" <> e ]

              Right resultSexp -> do
                tell [ "jsonToMidnightSexpNoFlatten:\n" <> show resultSexp ]

                case flattenSexpLists resultSexp of
                  Left e ->
                    tell [ "flattenSexp error:\n" <> e ]

                  Right flattened -> do
                    tell [ "flattenSexp:\n" <> show flattened ]

                    tell [ "final pretty:\n" <> (Sexp.prettyprintColsPrefer80 flattened) ]

    tell [ "-------------------------------------------" ]
