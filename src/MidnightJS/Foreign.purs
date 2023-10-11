module MidnightJS.Foreign where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Foreign (Foreign)

eval :: String -> Either String String
eval src =
  toString <$> evalToJson src

foreign import _evalToJson
  :: (forall x y. x -> Either x y)
  -> (forall x y. y -> Either x y)
  -> String
  -> Either String Json

evalToJson :: String -> Either String Json
evalToJson =
  _evalToJson Left Right

foreign import _evalToForeign
  :: (forall x y. x -> Either x y)
  -> (forall x y. y -> Either x y)
  -> String
  -> Either String Foreign

evalToForeign :: String -> Either String Foreign
evalToForeign =
  _evalToForeign Left Right

foreign import _applyClosure
  :: (forall x y. x -> Either x y)
  -> (forall x y. y -> Either x y)
  -> Foreign
  -> Array Foreign
  -> Either String Foreign

applyClosure :: Foreign -> Array Foreign -> Either String Foreign
applyClosure =
  _applyClosure Left Right

foreign import _toString
  :: Json
  -> String

toString :: Json -> String
toString =
  _toString
