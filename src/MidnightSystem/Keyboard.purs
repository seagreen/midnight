module MidnightSystem.Keyboard where

import Prelude

import Data.Enum (fromEnum)
import Data.List ((:))
import Data.List as PsList
import MidnightLang.Sexp (Sexp)
import MidnightLang.Sexp as Sexp

type Keyboard =
  { key :: Key
  , ctrlOrMeta :: Boolean
  }

data Key
  = KeyChar Char
  | KeyEnter
  | KeyBackspace
  | KeyArrow ArrowKey
  | KeyPageUp
  | KeyPageDown

data ArrowKey
  = ArrowUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight

aKey :: Keyboard
aKey =
  noMeta (KeyChar 'a')

noMeta :: Key -> Keyboard
noMeta key =
  { key, ctrlOrMeta: false }

toSexp :: Keyboard -> Sexp
toSexp keyInput =
  case keyInput.key of
    KeyChar c ->
      Sexp.List (Sexp.Symbol "key-char" : ctrlOrMeta : Sexp.Int (fromEnum c) : PsList.Nil)

    KeyEnter ->
      Sexp.List (Sexp.Symbol "key-enter" : ctrlOrMeta : PsList.Nil)

    KeyBackspace ->
      Sexp.List (Sexp.Symbol "key-backspace" : ctrlOrMeta : PsList.Nil)

    KeyArrow arrow ->
      case arrow of
        ArrowUp ->
          Sexp.List (Sexp.Symbol "key-arrow-up" : ctrlOrMeta : PsList.Nil)

        ArrowDown ->
          Sexp.List (Sexp.Symbol "key-arrow-down" : ctrlOrMeta : PsList.Nil)

        ArrowLeft ->
          Sexp.List (Sexp.Symbol "key-arrow-left" : ctrlOrMeta : PsList.Nil)

        ArrowRight ->
          Sexp.List (Sexp.Symbol "key-arrow-right" : ctrlOrMeta : PsList.Nil)

    KeyPageUp ->
      Sexp.List (Sexp.Symbol "key-page-up" : ctrlOrMeta : PsList.Nil)

    KeyPageDown ->
      Sexp.List (Sexp.Symbol "key-page-down" : ctrlOrMeta : PsList.Nil)

  where
  ctrlOrMeta :: Sexp
  ctrlOrMeta =
    if keyInput.ctrlOrMeta then
      Sexp.Symbol "ctrl-or-meta"
    else
      Sexp.Symbol "no-ctrl-or-meta"

toBiwa :: Keyboard -> String
toBiwa keyInput =
  Sexp.print
    ( Sexp.List
        ( Sexp.Symbol "quote"
            : toSexp keyInput
            : PsList.Nil
        )
    )
