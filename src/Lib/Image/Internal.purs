module Lib.Image.Internal where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data Image
  = Circle (Maybe Id) Number
  | Rectangle (Maybe Id) { width :: Number, height :: Number }
  | Line Point Point
  | Path (Array Point)
  | Text String
  | Style Style Image
  | Translate { x :: Number, y :: Number } Image
  | Overlay Image Image
  | Empty

derive instance Eq Image
derive instance Generic Image _
instance Show Image where
  show a = genericShow a

instance Semigroup Image where
  append i1 i2 =
    Overlay i1 i2

instance Monoid Image where
  mempty =
    Empty

type Point =
  { x :: Number
  , y :: Number
  }

type Style =
  { fill :: Maybe String
  , line :: Maybe String
  , text :: Maybe String
  }

newtype Id = Id String

derive instance Eq Id
derive instance Generic Id _
instance Show Id where
  show a = genericShow a
