module MidnightSystem.Startup where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.List ((:))
import Data.List as PsList
import Foreign (Foreign, unsafeToForeign)
import MidnightJS as MidnightJS
import MidnightJS.Foreign as Foreign
import MidnightLang.Sexp as Sexp
import MidnightSystem.SystemForeign as SystemForeign

editorStringToInput :: String -> Either String Foreign
editorStringToInput sourceStrPs = do
  bimap
    (\err -> "Evaluation of editorStringToInput failed: " <> err)
    unsafeToForeign
    (SystemForeign.editorStringToInput sourceStrPs)
