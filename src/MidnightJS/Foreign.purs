module MidnightJS.Foreign where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Foreign (Foreign)

eval :: String -> Either String String
eval src =
  toString <$> evalToForeign src

foreign import _evalToForeign
  :: (forall x y. x -> Either x y)
  -> (forall x y. y -> Either x y)
  -> String
  -> Either String Json

evalToForeign :: String -> Either String Json
evalToForeign =
  _evalToForeign Left Right

foreign import _toString
  :: Json
  -> String

toString :: Json -> String
toString =
  _toString
