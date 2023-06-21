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
      , HH.h2_
          [ HH.text "Examples" ]
      , HH.p_
          [ UiComponent.internalLink' Nav { route: Route.HelloWorld, label: HH.text "Hello World" } ]
      , HH.p_
          [ UiComponent.internalLink' Nav { route: Route.Editor, label: HH.text "Editor" } ]
      , HH.h2_
          [ HH.text "Related Projects" ]
      , HH.p_
          [ HH.a
              [ HP.href "https://github.com/seagreen/ians-lists/blob/main/miniature-computing-systems.md" ]
              [ HH.text "List of miniature computing systems" ]
          ]
      , HH.h2_
          [ HH.text "Motivation" ]
      , HH.p_
          [ HH.text "I find self-modifying, "
          , HH.a [ HP.href "https://en.wikipedia.org/wiki/Smalltalk#Image-based_persistence" ] [ HH.text "image" ]
          , HH.text " based systems intriguing. However they involve multiple concepts that are hard to understand:"
          , HH.ul_
              [ HH.li_ [ HH.text "Images vs. filesystems" ]
              , HH.li_ [ HH.text "What can and can't be modified" ]
              , HH.li_ [ HH.text "Which parts must be written in the base language vs which parts can be higher level" ]
              ]
          ]
      , HH.p_
          [ HH.text "In these situations I find the best move is to make a toy example of the thing I'm studying. Once I understand that it's easier to move on to the real thing." ]
      , HH.h2_
          [ HH.text "The Two-Key Turing Machine" ]
      , HH.p_
          [ HH.text "The most minimal self-modifying system would be a turing tape, a boolean for a display, and a boolean for input." ]
      , HH.p_
          [ HH.text "A physical version would look like this with a lightbulb attached:" ]
      , HH.img
          [ HP.src "binary-keyboard.png"
          , HP.width 300
          , HP.height 240
          , HP.alt "Binary keyboard with just 0 and 1 keys."
          ]
      , HH.p_
          [ HH.text "That's a little stark though, so I expanded my criteria to be both (1) minimal and (2) high-level." ]
      , HH.h2_
          [ HH.text "The Result" ]
      , HH.p_
          [ HH.text "I decided to use a Lisp variant as the base language. It's easy to make a Lisp that's both tiny and readable. More details are in "
          , UiComponent.internalLink' Nav { route: Route.MidnightLispDoc, label: HH.text "the language doc" }
          , HH.text "."
          ]
      , HH.p_ [ HH.text "With that done the rest of the project fell into place." ]
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
                  , HH.text " a Midnight Lisp value, which unlike an S-expression can include closures."
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
                  , HH.text " Midnight Lisp."
                  ]
              ]
          , HH.li_
              [ HH.p_
                  [ HH.span
                      [ HP.style "font-weight: bold" ]
                      [ HH.text "High-level language:" ]
                  , HH.text " none specified, but they can be built within the system."
                  , HH.text " "
                  , UiComponent.internalLink' Nav { route: Route.Editor, label: HH.text "Editor" }
                  , HH.text " gives an example of this."
                  ]
              ]
          ]
      , HH.h2_
          [ HH.text "Long-Term Goals" ]
      , HH.p_
          [ HH.text "Roughly in order:" ]
      , HH.ol_
          [ HH.li_
              [ HH.text "Don't change. Make a precise specification and freeze it." ]
          , HH.li_
              [ HH.text "Be high-level and comprehensible" ]
          , HH.li_
              [ HH.text "Be small. Where this conflicts with comphensibility the latter is favored. For example, the language has let even though it can be viewed as sugar for function application." ]
          , HH.li_
              [ HH.text "Be performant enough to be fun." ]
          ]
      ]
