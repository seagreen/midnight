module App.Home where

import Prelude

import App.Capability.Navigate (class Navigate, navTo)
import App.Halogen (OpaqueSlot)
import App.Route (Route)
import App.Route as Route
import App.UiComponent as UiComponent
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
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
          [ HH.text "What is This?" ]
      , HH.p_
          [ HH.text "A specification for a miniature, Scheme-like language ("
          , HH.span
              [ HP.style "font-weight: bold" ]
              [ HH.text "*Midnight Lisp*" ]
          , HH.text ") and a computing environment based on it ("
          , HH.span
              [ HP.style "font-weight: bold" ]
              [ HH.text "*The Midnight System*" ]
          , HH.text ")."
          ]
      , HH.p_
          [ HH.text "The Midnight System's input is key presses. Its output is lines and text."
          ]
      , HH.p_
          [ HH.text "There are no packages and no FFI, so each program is its own world.🏝️"
          ]
      , HH.h2_ [ HH.text "Words are Bad, Let Me See It" ]
      , HH.p_
          [ UiComponent.internalLink' Nav { route: Route.HelloWorld, label: HH.text "Hello World" } ]
      , HH.h2_
          [ HH.text "Why" ]
      , HH.ul_
          [ HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Fun Through Comprehensibility:" ]
                  , HH.text " With Midnight you can understand absolutely everything about the system you're using in about an hour. This is totally unlike modern software development (for most of us, at least). It's kind of neat."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Education:" ]
                  , HH.text " Building up from near-scratch is an insanely effective way to learn."
                  ]
              ]
          ]
      , HH.h2_
          [ HH.text "Docs in Brief" ]
      , HH.h3_
          [ HH.text "The Language" ]
      , HH.p_
          [ HH.text "lambda, let, quote, eval, if, car, cdr, cons, list-empty?, list-nonempty?, symbol? symbol-eq?, codepoints-to-symbol, int?, +, -, *, /, <, =, >" ]
      , HH.h3_
          [ HH.text "The System" ]
      , HH.ul_
          [ HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Persistent storage:" ]
                  , HH.text " an S-expression."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Ephemeral storage:" ]
                  , HH.text " a Midnight value, which unlike an S-expression can include closures."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Input:" ]
                  , HH.text " key presses."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Output:" ]
                  , HH.text " black lines and text on a white background."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "\"Machine\" language:" ]
                  , HH.text " Midnight."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "High-level language:" ]
                  , HH.text " you probably want to build your own."
                  , HH.text " "
                  , UiComponent.internalLink' Nav { route: Route.Editor, label: HH.text "Editor" }
                  , HH.text " gives an example of this."
                  ]
              ]
          ]
      , HH.h2_
          [ HH.text "Non-Goals" ]
      , HH.ul_
          [ HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Be for Everyone:" ]
                  , HH.text " The target audience is niche. Even if you like the idea you might have more fun building your own platform too."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "Be Good:" ]
                  , HH.text " Midnight is minimalistic, not in the sense of \"elegant\", but in the sense of \"too small\". Turns out colored output and a data structure that's not a linked-list are both boons! But Midnight is optimized for comprehensibility, not goodness."
                  ]
              ]
          ]
      , HH.h2_
          [ HH.text "Goals" ]
      , HH.p_
          [ HH.text "Roughly in order:" ]
      , HH.ol_
          [ HH.li_
              [ HH.text "Don't change. Make a precise specification and freeze it." ]
          , HH.li_
              [ HH.text "Be comprehensible" ]
          , HH.li_
              [ HH.text "Be tiny and high level. There's already a horde of projects in the other quadrants. Where this conflicts with comphensibility the latter is favored. For example, the language has let even though it can be viewed as sugar for function application." ]
          , HH.li_
              [ HH.text "Stick roughly to Scheme conventions. car cdr cons, lexical scope, etc. This way (a) we don't have to reinvent that wheel, and (b) Midnight knowledge can carry over a little bit to a real language." ]
          , HH.li_
              [ HH.text "Be performant enough to be fun." ]
          ]
      , HH.h2_
          [ HH.text "Examples" ]
      , HH.p_
          [ UiComponent.internalLink' Nav { route: Route.HelloWorld, label: HH.text "Hello World" } ]
      , HH.p_
          [ UiComponent.internalLink' Nav { route: Route.Editor, label: HH.text "Editor" } ]

      , HH.h2_
          [ HH.text "Related Projects" ]
      , HH.p_
          [ HH.text "Coming soon, I'm going to make a list of these." ]
      ]
