module App.Page.MidnightLispDoc where

import Prelude

import App.Capability.Navigate (class Navigate, navTo)
import App.Halogen (OpaqueSlot)
import App.Route (Route)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action = Nav Route MouseEvent

type ChildSlots =
  ( midnightEditor :: OpaqueSlot Unit
  )

component :: forall q i o m. Navigate m => MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> unit
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
  handleAction :: forall state slots. Action -> H.HalogenM state Action slots o m Unit
  handleAction = case _ of
    Nav route event -> do
      navTo route (MouseEvent.toEvent event)

  render :: forall state. state -> H.ComponentHTML Action ChildSlots m
  render _ =
    HH.div_
      [ HH.h2_
          [ HH.text "Midnight Lisp" ]
      , HH.h3_
          [ HH.text "Primitives" ]
      , HH.p_
          [ HH.text "lambda, let, quote, eval, if, car, cdr, cons, pair?, list-empty?, symbol? symbol-eq?, codepoints-to-symbol, int?, +, -, *, /, <, =, >" ]
      , HH.h3_
          [ HH.text "Goals" ]
      , HH.ul_
          [ HH.li_
              [ HH.text "Stick roughly to Scheme conventions. car cdr cons, lexical scope, etc. This way (a) we don't have to reinvent that wheel, and (b) Midnight knowledge can carry over a little bit to a real language." ]
          ]
      ]
