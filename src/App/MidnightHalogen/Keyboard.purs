module App.MidnightHalogen.Keyboard where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import MidnightSystem.Keyboard (ArrowKey(..), Key(..), Keyboard)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

toMidnightKey :: KeyboardEvent -> Maybe Keyboard
toMidnightKey ev = do
  key <- toMidnightKeyNoModifier ev
  pure { key, ctrlOrMeta }
  where
  ctrlOrMeta :: Boolean
  ctrlOrMeta =
    KE.ctrlKey ev || KE.metaKey ev

toMidnightKeyNoModifier :: KeyboardEvent -> Maybe Key
toMidnightKeyNoModifier ev = do
  { head } <- CodeUnits.uncons charStr
  if String.length charStr == 1 && intercept then
    Just (KeyChar head)
  else
    case charStr of
      "Enter" ->
        Just KeyEnter

      "Backspace" ->
        Just KeyBackspace

      "ArrowUp" ->
        Just (KeyArrow ArrowUp)

      "ArrowDown" ->
        Just (KeyArrow ArrowDown)

      "ArrowLeft" ->
        Just (KeyArrow ArrowLeft)

      "ArrowRight" ->
        Just (KeyArrow ArrowRight)

      "PageUp" ->
        Just KeyPageUp

      "PageDown" ->
        Just KeyPageDown

      "Tab" ->
        Just KeyTab

      _ ->
        Nothing
  where
  -- Eg we don't intercept ctrl-r
  intercept :: Boolean
  intercept =
    not (KE.ctrlKey ev && KE.code ev == "KeyR")

  charStr :: String
  charStr =
    KE.key ev

-- | When on the "source" tab
isRelaunchRequested :: KeyboardEvent -> Boolean
isRelaunchRequested ev =
  charStr == "Enter" && ctrlOrMeta
  where
  charStr :: String
  charStr =
    KE.key ev

  ctrlOrMeta :: Boolean
  ctrlOrMeta =
    KE.ctrlKey ev || KE.metaKey ev
