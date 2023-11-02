module MidnightSystem.StartFromSource where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either)
import Foreign (Foreign, unsafeToForeign)
import MidnightSystem.SystemForeign as SystemForeign

editorStringToInput :: String -> Either String Foreign
editorStringToInput sourceStrPs = do
  bimap
    (\err -> "Evaluation of editorStringToInput failed: " <> err)
    unsafeToForeign
    (SystemForeign.editorStringToInput sourceStrPs)
