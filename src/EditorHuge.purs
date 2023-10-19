module EditorHuge
  ( string
  ) where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String as String
import Generated.EditorSource as EditorNormal
import Lib.Debug (crash)

string :: Int -> String
string n =
  stringStart <> stringMiddle n <> stringEnd

stringStart :: String
stringStart =
  """(let
  ((midnight-plus-macros
    '(
"""

stringMiddle :: Int -> String
stringMiddle n =
  foldMap
    (\n -> "(define foo" <> show n <> example)
    (1 .. n)
  where
  example :: String
  example =
    """
  'impl
    (lambda (editor)
      (let
        ((cursor
          (string-concat
            (list
              "cursor: "
              (int->string (grid-posn-column (editor-cursor editor)))
              ", "
              (int->string (grid-posn-row (editor-cursor editor))))))

          (line-count
            (string-append
              "lines: "
              (int->string (editor-line-count editor)))))
        (string-pad-center
          80
          cursor
          line-count))))
"""

stringEnd :: String
stringEnd =
  case String.stripPrefix (String.Pattern stringStart) EditorNormal.string of
    Nothing ->
      crash "EditorHuge.string: failed to strip stringStart"

    Just s ->
      s
