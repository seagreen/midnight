module MidnightJS.JsonToMidnightSexp where

import Prelude

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
  flattenSexpLists =<< jsonToMidnightSexpNoFlatten json

jsonToMidnightSexpNoFlatten :: Json -> Either String Sexp
jsonToMidnightSexpNoFlatten json =
  caseJson
    (\_ -> Left "unexpected null")
    (\b -> Left ("unexpected bool: " <> show b))
    ( \n -> case fromNumber n of
        Nothing -> Left ("not an integer: " <> show n)
        Just int -> Right (Sexp.Int int)
    )
    (Right <<< Sexp.Symbol)
    (\xs -> Sexp.List <<< List.fromFoldable <$> for xs jsonToMidnightSexpNoFlatten)
    (\_ -> Left ("unexpected object: " <> stringify json))
    json

flattenSexpLists :: Sexp -> Either String Sexp
flattenSexpLists sexp =
  case sexp of
    Sexp.List _ ->
      Sexp.List <$> flattenList sexp

    _ ->
      pure sexp

flattenList :: Sexp -> Either String (List Sexp)
flattenList xs =
  case xs of
    Sexp.List List.Nil ->
      pure List.Nil

    Sexp.List (car : cdr : List.Nil) -> do
      next <- flattenSexpLists car
      rest <- flattenList cdr
      pure (List.Cons next rest)

    _ ->
      Left
        ( "flattenList expected a pair, but got: "
            <> show xs
        )
