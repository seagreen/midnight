module App.UiComponent.Toggle where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

toggle :: forall a cs m. Boolean -> (Boolean -> a) -> H.ComponentHTML a cs m
toggle b toAction =
  HH.button
    [ HE.onClick (\_ -> toAction (not b)) ]
    [ HH.text (if b then "On" else "Off") ]
