module App.UiComponent.InternalLink where

import Prelude

import App.Route (Route)
import App.Route as Route
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Duplex as RD
import Web.UIEvent.MouseEvent (MouseEvent)

internalLink
  :: forall action w
   . (Route -> MouseEvent -> action)
  -> Route
  -> HTML w action
internalLink toAction route =
  internalLink' toAction
    { route: route
    , label: HH.text (RD.print Route.codec route)
    }

internalLink'
  :: forall action w
   . (Route -> MouseEvent -> action)
  -> { route :: Route, label :: HTML w action }
  -> HTML w action
internalLink' toAction a =
  HH.a
    [ HP.href (RD.print Route.codec a.route)
    , HE.onClick (toAction a.route)
    ]
    [ a.label ]
