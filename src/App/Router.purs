module App.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navTo)
import App.Halogen (OpaqueSlot)
import App.MidnightHalogen as App.MidnightHalogen
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
    HH.div
      [HP.class_ (H.ClassName "max-sm:ml-1 md:flex md:flex-col md:items-center mt-4")]
      [ HH.h1
          [HP.class_ (H.ClassName "text-xl font-bold")]
          linkToHomeIfWereNotThere
      , case route of
          Just r -> case r of
            Route.Home ->
              HH.slot_
                (Proxy :: _ "home")
                unit
                (App.MidnightHalogen.component EditorSource.string)
                unit

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
          -- TODO: make blue
          [ UiComponent.internalLink' Nav { route: Route.Home, label: HH.text "Midnight" }
          , HH.text " System"
          ]
