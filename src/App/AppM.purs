module App.AppM where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Route as Route
import Control.Monad.Reader (class MonadReader)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import Halogen as H
import Halogen.Component as HC
import Routing.Duplex as RD
import Routing.PushState (PushStateInterface)
import Simple.JSON as SimpleJson
import Web.Event.Event as WebEvent

newtype AppM a = AppM (ReaderT Env Aff a)

newtype Env = Env PushStateInterface

runAppM :: forall q i o. Env -> H.Component q i o AppM -> H.Component q i o Aff
runAppM env =
  HC.hoist (\(AppM m) -> runReaderT m env)

derive newtype instance Functor AppM

derive newtype instance Apply AppM
derive newtype instance Applicative AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM

derive newtype instance MonadAsk Env AppM
derive newtype instance MonadReader Env AppM

instance Navigate AppM where
  navTo route event = do
    liftEffect $ WebEvent.preventDefault event
    Env nav <- ask
    liftEffect $ nav.pushState (SimpleJson.write {} :: Foreign)
      (RD.print Route.codec route)
