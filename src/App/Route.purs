module App.Route where

import Prelude hiding ((/))

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', as, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug

data Route
  = Home

derive instance Generic Route _
derive instance Eq Route
derive instance Ord Route

codec :: RouteDuplex' Route
codec =
  root $ sum
    { "Home": noArgs
    }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug =
  as Slug.toString (Slug.parse >>> note "Bad slug")
