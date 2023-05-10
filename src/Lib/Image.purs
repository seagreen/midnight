module Lib.Image
  ( module Lib.Image.Internal
  , module Lib.Image
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Lib.Image.Internal (Id(..), Image(..), Point, Style)

circle :: Maybe Id -> Number -> Image
circle id r =
  Circle id r

square :: Maybe Id -> Number -> Image
square id width =
  Rectangle id
    { width: width
    , height: width
    }

rectangle :: Maybe Id -> { width :: Number, height :: Number } -> Image
rectangle id a =
  Rectangle id a

line :: Point -> Point -> Image
line p1 p2 =
  Line p1 p2

path :: Array Point -> Image
path ps =
  Path ps

text :: String -> Image
text s =
  Text s

style :: Style -> Image -> Image
style s =
  Style s

defStyle :: Style
defStyle =
  { fill: Nothing
  , line: Nothing
  , text: Nothing
  }

colorFill :: Maybe String -> Image -> Image
colorFill color =
  Style defStyle { fill = color }

colorLine :: Maybe String -> Image -> Image
colorLine color =
  Style defStyle { line = color }

colorText :: Maybe String -> Image -> Image
colorText color =
  Style defStyle { text = color }

translate :: { x :: Number, y :: Number } -> Image -> Image
translate xy =
  Translate xy
