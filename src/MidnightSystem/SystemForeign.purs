module MidnightSystem.SystemForeign where

import Prelude

import Data.Argonaut (Json)
import Data.Either (Either(..))
import Foreign (Foreign)

foreign import _editorStringToInput
  :: (forall x y. x -> Either x y)
  -> (forall x y. y -> Either x y)
  -> String
  -> Either String Json

editorStringToInput :: String -> Either String Json
editorStringToInput =
  _editorStringToInput Left Right
