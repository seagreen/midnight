module MidnightBiwa where

import Prelude

import Data.Either (Either)
import Data.Traversable (for)
import Foreign (Foreign)
import MidnightBiwa.Foreign as BiwaForeign
import MidnightBiwa.Translate as Translate
import MidnightLang.Sexp (Sexp)

eval :: String -> Either String String
eval midnightSrc = do
  biwa <- Translate.midnightToBiwaPlusSnippet midnightSrc
  val <- BiwaForeign.eval biwa
  Translate.biwaToMidnight val

evalToSexp :: String -> Either String Sexp
evalToSexp midnightSrc = do
  biwa <- Translate.midnightToBiwaPlusSnippet midnightSrc
  val <- BiwaForeign.eval biwa
  Translate.biwaToMidnightSexp val

evalToForeign :: String -> Either String Foreign
evalToForeign midnightSrc = do
  biwa <- Translate.midnightToBiwaPlusSnippet midnightSrc
  BiwaForeign.evalToForeign biwa

applyClosure :: Foreign -> Array Foreign -> Either String Foreign
applyClosure =
  BiwaForeign.applyClosure

applyClosureFirstEvalingArgs :: Foreign -> Array String -> Either String Foreign
applyClosureFirstEvalingArgs l args = do
  argsMidnight <- for args Translate.midnightToBiwaPlusSnippet
  BiwaForeign.applyClosureFirstEvalingArgs l argsMidnight

toBiwaString :: Foreign -> String
toBiwaString =
  BiwaForeign.toString
