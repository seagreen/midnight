module MidnightSystem.Startup where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.List ((:))
import Data.List as PsList
import Foreign (Foreign, unsafeToForeign)
import MidnightBiwa as MidnightBiwa
import MidnightBiwa.Foreign as Foreign
import MidnightLang.Sexp as Sexp

editorStringToInput :: String -> Either String Foreign
editorStringToInput str = do
  f <-
    lmap
      (\err -> "Evaluation of editorStringToInput failed: " <> err)
      (Foreign.evalToForeign editorStringToInputBiwaCode)

  -- TODO: Can we get rid of unsafeToForeign?
  MidnightBiwa.applyClosure f [ unsafeToForeign str ]

editorStringToInputBiwaCode :: String
editorStringToInputBiwaCode =
  """
(lambda (str)
  `(system-input-start-with-editor-contents-midnight
      (string-tag-midnight ,(map char->integer (string->list str)))))
"""

editorStringToInputReference :: String -> Either String Foreign
editorStringToInputReference str =
  MidnightBiwa.evalToForeign midnightStr
  where
  midnightStr :: String
  midnightStr =
    -- Using `Sexp.print` instead of `Translate.midnightSexpToBiwaNoSnippet`
    -- means we don't double-encode to the target language, first here and
    -- then above in `MidnightBiwa.evalToForeign`.
    --
    -- Note that there's still a snippet added, since `MidnightBiwa.evalToForeign`
    -- adds one.
    Sexp.print
      ( Sexp.List
          ( Sexp.Symbol "quote"
              :
                ( Sexp.List
                    ( Sexp.Symbol "system-input-start-with-editor-contents"
                        : Sexp.purescriptStringToMidnightStringNoQuote str
                        : PsList.Nil
                    )
                )
              : PsList.Nil
          )
      )
