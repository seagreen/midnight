module MidnightJS.JsonToSexp where

import Prelude

import Data.Argonaut (Json, caseJson, stringify)
import Data.Either (Either(..))
import Data.Int (fromNumber)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

jsonToSexp :: Json -> Either String Sexp
jsonToSexp json =
  caseJson
    (\_ -> Left "unexpected null")
    (\b -> Left ("unexpected bool: " <> show b))
    ( \n -> case fromNumber n of
        Nothing -> Left ("not an integer: " <> show n)
        Just int -> Right (Sexp.Int int)
    )
    (Right <<< Sexp.Symbol)
    (\xs -> Sexp.List <<< List.fromFoldable <$> for xs jsonToSexp)
    (\_ -> Left ("unexpected object: " <> stringify json))
    json
