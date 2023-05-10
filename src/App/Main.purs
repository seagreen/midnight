module App.Main where

import Prelude

import App.AppM (Env(..), runAppM)
import App.Route as Route
import App.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as RD
import Routing.PushState as PushState

main :: Effect Unit
main = HA.runHalogenAff do
  liftEffect $ Console.log "Starting"
  body <- HA.awaitBody
  nav <- liftEffect $ PushState.makeInterface
  let router = runAppM (Env nav) Router.component
  halogenIO <- runUI router unit body
  void $ liftEffect $ PushState.matchesWith (RD.parse Route.codec)
    ( \old new -> do
        when (old /= Just new) do
          -- NOTE: The `$ void $` isn't in purescript-halogen-realworld:
          launchAff_ $ void $ halogenIO.query $ H.mkTell $ Router.NavigateTo new
    )
    nav
