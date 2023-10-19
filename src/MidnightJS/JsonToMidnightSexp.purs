module MidnightJS.JsonToMidnightSexp where

import Prelude

import Control.Monad.Except.Trans (ExceptT, except, runExceptT)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Argonaut (Json, caseJson, stringify)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

jsonToMidnightSexp :: Json -> Either String Sexp
jsonToMidnightSexp json = do
  midnightSexp <- jsonToMidnightSexpNoFlatten json
  flattenSexpLists midnightSexp

-- | jsonToMidnightSexpNoFlatten uses tailRecM
--
-- See PureScript: Jordan's Reference
-- https://jordanmartinez.github.io/purescript-jordans-reference-site/content/31-Design-Patterns/23-Stack-Safety/01-Explicit-TCO.html
--
-- and Taming the stack by Nathan Faubion:
-- https://hasgeek.com/FP_Juspay/pureconf/schedule/taming-the-stack-5RMhApk2iAEoEaRr844z14
--
data ParseCall
  = ParseGo Json Accum
  | ParseEval Sexp Accum

data Accum
  = ContLhs Json Accum
  | ContRhs Sexp Accum
  | ContIdentity

jsonToMidnightSexpNoFlatten :: Json -> Either String Sexp
jsonToMidnightSexpNoFlatten =
  (\json -> tailRecM go (ParseGo json ContIdentity))
  where
  go :: ParseCall -> Either String (Step ParseCall Sexp)
  go =
    case _ of
      ParseGo json cont ->
        caseJson
          ( \_ ->
              Left "unexpected null"
          )
          ( \b ->
              Left ("unexpected bool: " <> show b)
          )
          ( \n -> case fromNumber n of
              Nothing ->
                Left ("not an integer: " <> show n)
              Just int ->
                pure (Loop (ParseEval (Sexp.Int int) cont))
          )
          ( \str ->
              pure (Loop (ParseEval (Sexp.Symbol str) cont))
          )
          ( \xs ->
              case List.fromFoldable xs of
                List.Nil ->
                  pure (Loop (ParseEval (Sexp.List List.Nil) cont))

                x : rest : List.Nil -> do
                  pure (Loop (ParseGo x (ContLhs rest cont)))

                _ ->
                  Left ("Not a zero or two element list: " <> stringify json)
          )
          ( \_ ->
              Left ("unexpected object: " <> stringify json)
          )
          json

      ParseEval sexp cont ->
        case cont of
          ContLhs rhsJson next ->
            pure (Loop (ParseGo rhsJson (ContRhs sexp next)))

          ContRhs lhsSexp next ->
            pure (Loop (ParseEval (Sexp.List (lhsSexp : sexp : List.Nil)) next))

          ContIdentity ->
            pure (Done sexp)

flattenSexpLists :: Sexp -> Either String Sexp
flattenSexpLists sexp =
  case sexp of
    Sexp.List _ -> do
      ys <- flattenSingleList sexp
      Sexp.List <$> for ys flattenSexpLists

    _ ->
      pure sexp

flattenSingleList :: Sexp -> Either String (List Sexp)
flattenSingleList sexp =
  case sexp of
    Sexp.List List.Nil ->
      pure List.Nil

    Sexp.List (car : cdr : List.Nil) -> do
      rest <- flattenSingleList cdr
      pure (List.Cons car rest)

    _ ->
      Left
        ( "flattenList expected a pair, but got: "
            <> show sexp
        )
