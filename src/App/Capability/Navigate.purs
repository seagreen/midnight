module App.Capability.Navigate where

import Prelude

import App.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Web.Event.Event as WebEvent

class Monad m <= Navigate m where
  navTo :: Route -> WebEvent.Event -> m Unit

instance Navigate m => Navigate (HalogenM st act slots msg m) where
  navTo route event = lift (navTo route event)
