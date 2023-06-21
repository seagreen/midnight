module App.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navTo)
import App.Halogen (OpaqueSlot)
import App.Home as Home
import App.ImageExample as ImageExample
import App.MidnightHalogen as App.MidnightHalogen
import App.Page.MidnightLispDoc as Page.MidnightLispDoc
import App.Route (Route)
import App.Route as Route
import App.UiComponent as UiComponent
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Generated.EditorSource as EditorSource
import Generated.HelloWorldSource as HelloWorldSource
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Query a = NavigateTo Route a

type State =
  { route :: Maybe Route
  }

data Action = Nav Route MouseEvent

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , midnightLispDoc :: OpaqueSlot Unit
  , helloWorld :: OpaqueSlot Unit
  , editor :: OpaqueSlot Unit
  , image :: OpaqueSlot Unit
  )

component
  :: forall m
   . MonadAff m
  => Navigate m
  => H.Component Query Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Nothing
        , handleQuery = handleQuery
        , handleAction = handleAction
        }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Nav route event -> do
      navTo route (MouseEvent.toEvent event)

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    NavigateTo dest a -> do
      { route } <- H.get
      -- don't re-render unnecessarily if the route is unchanged:
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } =
    HH.div_
      [ HH.h1_ linkToHomeIfWereNotThere
      , constructionNoticeIfNotOnPage
      , case route of
          Just r -> case r of
            Route.Home ->
              HH.slot_ (Proxy :: _ "home") unit Home.component unit

            Route.MidnightLispDoc ->
              HH.slot_ (Proxy :: _ "midnightLispDoc") unit Page.MidnightLispDoc.component unit

            Route.HelloWorld ->
              HH.slot_
                (Proxy :: _ "helloWorld")
                unit
                (App.MidnightHalogen.component "Hello World" HelloWorldSource.string)
                unit

            Route.Editor ->
              HH.slot_
                (Proxy :: _ "editor")
                unit
                (App.MidnightHalogen.component "Editor" EditorSource.string)
                unit

            Route.Image ->
              HH.slot_ (Proxy :: _ "image") unit ImageExample.component unit

          Nothing ->
            HH.div_ [ HH.text "Page not found." ]
      ]
    where
    linkToHomeIfWereNotThere :: forall w. Array (HTML w Action)
    linkToHomeIfWereNotThere =
      case route of
        Just Route.Home ->
          [ HH.text "Midnight System"
          ]

        _ ->
          [ UiComponent.internalLink' Nav { route: Route.Home, label: HH.text "Midnight" }
          , HH.text " System"
          ]

    constructionNoticeIfNotOnPage :: H.ComponentHTML Action ChildSlots m
    constructionNoticeIfNotOnPage =
      case route of
        Just Route.Home ->
          HH.div_ []

        Just Route.MidnightLispDoc ->
          HH.div_ []

        _ ->
          HH.div_
            [ HH.p_
                [ HH.text "⚠️Construction notice: see "
                , HH.a
                    [ HP.href "https://github.com/seagreen/midnight/issues/1" ]
                    [ HH.text "this issue" ]
                , HH.text " for details"
                ]
            ]
