module MidnightSystem.Output where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodePoints (CodePoint)
import Data.Traversable (for)
import Foreign (Foreign)
import Lib.Image (Image)
import Lib.Sexp as GenericSexp
import MidnightBiwa as MidnightBiwa
import MidnightBiwa.Foreign as Foreign
import MidnightBiwa.Translate as Translate
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp
import MidnightSystem.Image as MidnightSystem.Image

data StepOutput = StepNormal { imageSexp :: Sexp, image :: Image, store :: Foreign, ephem :: Foreign }

biwaOutput :: Foreign -> Either String StepOutput
biwaOutput val = do
  outputConstructorForeign <- getFirst val
  outputConstructorSexp <- foreignToSexp outputConstructorForeign
  case outputConstructorSexp of
    Sexp.Symbol "output-normal" -> do
      imageForeign <- getSecond val
      imageSexp <- lmap (\err -> "foreign to image sexp: " <> err) (foreignToSexp imageForeign)
      image <- lmap (\err -> "parse image sexp: " <> err) (MidnightSystem.Image.parse imageSexp)
      store <- getThird val
      ephem <- getFourth val
      pure (StepNormal { imageSexp, image, store, ephem })

    other ->
      Left ("Parse step output: output constructor not recognized: " <> Sexp.print other)

-- | The argument to `output-restart` can be so big that it crashes
-- | when we try to turn it into a string.
-- | For this reason we can't use `Sexp` as a parameter to this function,
-- | but must use `Foreign` instead.
stringForeignToString :: Foreign -> Either String String
stringForeignToString val = do
  l <- Foreign.evalToForeign biwaStringExtractor
  ints <- Foreign.applyClosure l [ val ]
  let psInts = Foreign.toArrayInt ints :: Array Int
  codepoints <-
    for psInts
      ( \n ->
          case toEnum n of
            Nothing ->
              Left ("int not codepoint: " <> show n)

            Just a ->
              Right a
      )
  pure (String.fromCodePointArray (codepoints :: Array CodePoint))

-- | NOTE: This is BiwaScheme, not Midnight.
-- | This is in order to use the `list->js-array` BiwaScheme function.
biwaStringExtractor :: String
biwaStringExtractor =
  """
(lambda (str)
  (if
    (pair? str)
    (if
      (eqv? 'string-tag-midnight (car str)) ; NOTE: the `-midnight` suffix
      (list->js-array (cadr str))
      crash-string-not-tagged-with-string-tag) ; TODO: How to crash more informatively in biwa
    crash-not-represented-as-tagged-list))
"""

foreignToSexp :: Foreign -> Either String Sexp
foreignToSexp biwaForeign = do
  biwaSexp <- GenericSexp.parse (MidnightBiwa.toBiwaString biwaForeign)
  Translate.biwaSexpToMidnightSexp biwaSexp

foreignToSexpAllowClosures :: Foreign -> Either String Sexp
foreignToSexpAllowClosures biwaForeign = do
  biwaSexp <- GenericSexp.parse (MidnightBiwa.toBiwaString biwaForeign)
  Translate.biwaSexpToMidnightSexp biwaSexp

getFirst :: Foreign -> Either String Foreign
getFirst biwa =
  lmap
    (\err -> "getFirst: " <> err)
    (applyStringToForeign "(lambda (x) (car x))" biwa)

getSecond :: Foreign -> Either String Foreign
getSecond biwa =
  lmap
    (\err -> "getSecond: " <> err)
    (applyStringToForeign "(lambda (x) (car (cdr x)))" biwa)

getThird :: Foreign -> Either String Foreign
getThird biwa =
  lmap
    (\err -> "getThird: " <> err)
    (applyStringToForeign "(lambda (x) (car (cdr (cdr x))))" biwa)

getFourth :: Foreign -> Either String Foreign
getFourth biwa =
  lmap
    (\err -> "getFourth: " <> err)
    (applyStringToForeign "(lambda (x) (car (cdr (cdr (cdr x)))))" biwa)

applyStringToForeign :: String -> Foreign -> Either String Foreign
applyStringToForeign str biwa = do
  l <- MidnightBiwa.evalToForeign str
  MidnightBiwa.applyClosure l [ biwa ]
