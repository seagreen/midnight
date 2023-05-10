module App.ImageExample where

import Prelude

import App.Capability.Navigate (class Navigate, navTo)
import App.Route (Route)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Lib.Image (Image)
import Lib.Image as Image
import Lib.ImageSvg as ImageSvg
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action
  = Nav Route MouseEvent
  | SvgOnClick Image.Id
  | SvgOnHover Image.Id
  | SvgOnExit Image.Id

-- | For the selection example.
type State =
  { lastClicked :: Maybe Image.Id
  , lastHover :: Maybe Image.Id
  }

component :: forall q i o m. Navigate m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ ->
        { lastClicked: Nothing
        , lastHover: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }

  where
  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    Nav route event -> do
      navTo route (MouseEvent.toEvent event)

    SvgOnClick id ->
      H.modify_ _ { lastClicked = Just id }

    SvgOnHover id ->
      H.modify_ _ { lastHover = Just id }

    SvgOnExit id -> do
      lastHover <- H.gets _.lastHover
      when (Just id == lastHover) (H.modify_ _ { lastHover = Nothing })

  render :: forall cs. State -> H.ComponentHTML Action cs m
  render state =
    HH.div_
      [ HH.h2_ [ HH.text "Image Library Examples" ]
      , circle
      , square
      , rectangle
      , line
      , path
      , text
      , interactive state
      , colorOverride
      ]

circle :: forall a cs m. H.ComponentHTML a cs m
circle =
  HH.div_
    [ HH.h3_ [ HH.text "Circle" ]
    , ImageSvg.toHalogenSvg ImageSvg.noEvent example
    ]
  where
  example :: Image
  example =
    Image.circle Nothing 20.0

square :: forall a cs m. H.ComponentHTML a cs m
square =
  HH.div_
    [ HH.h3_ [ HH.text "Square" ]
    , ImageSvg.toHalogenSvg ImageSvg.noEvent example
    ]
  where
  example :: Image
  example =
    Image.square Nothing 20.0

rectangle :: forall a cs m. H.ComponentHTML a cs m
rectangle =
  HH.div_
    [ HH.h3_ [ HH.text "Rectangle" ]
    , ImageSvg.toHalogenSvg ImageSvg.noEvent example
    ]
  where
  example :: Image
  example =
    Image.rectangle Nothing
      { width: 30.0
      , height: 10.0
      }

line :: forall a cs m. H.ComponentHTML a cs m
line =
  HH.div_
    [ HH.h3_ [ HH.text "Line" ]
    , ImageSvg.toHalogenSvg ImageSvg.noEvent example
    ]
  where
  example :: Image
  example =
    Image.line
      { x: -20.0, y: -20.0 }
      { x: 20.0, y: 20.0 }

path :: forall a cs m. H.ComponentHTML a cs m
path =
  HH.div_
    [ HH.h3_ [ HH.text "Path" ]
    , ImageSvg.toHalogenSvg ImageSvg.noEvent example
    ]
  where
  example :: Image
  example =
    Image.path
      [ { x: -20.0, y: -10.0 }
      , { x: -10.0, y: 10.0 }
      , { x: 0.0, y: -10.0 }
      , { x: 10.0, y: 10.0 }
      , { x: 20.0, y: -10.0 }
      ]

text :: forall a cs m. H.ComponentHTML a cs m
text =
  HH.div_
    [ HH.h3_ [ HH.text "Text" ]
    , ImageSvg.toHalogenSvg ImageSvg.noEvent example
    ]
  where
  example :: Image
  example =
    Image.text "hello explorer"
      <> Image.rectangle Nothing
        { width: 95.0
        , height: 30.0
        }

interactive :: forall cs m. State -> H.ComponentHTML Action cs m
interactive state =
  HH.div_
    [ HH.h3_ [ HH.text "Interactive" ]
    , ImageSvg.toHalogenSvg
        { onClick: Just (SvgOnClick <<< Image.Id)
        , onHover: Just (SvgOnHover <<< Image.Id)
        , onExit: Just (SvgOnExit <<< Image.Id)
        }
        (interactiveExampleImage state)
    ]

interactiveExampleImage :: State -> Image
interactiveExampleImage state =
  Image.style
    Image.defStyle
      { fill = blueFill
      , line = blueBorder
      }
    ( Image.circle
        (Just blueId)
        20.0
    )
    <> Image.translate { x: 30.0, y: 30.0 }
      ( Image.style
          Image.defStyle
            { fill = grayFill
            , line = grayBorder
            }
          ( Image.circle
              (Just grayId)
              5.0
          )
      )
  where
  blueId =
    Image.Id "blue-circle"

  grayId =
    Image.Id "gray-circle"

  blueFill =
    if state.lastHover == Just blueId then Just "deepskyblue"
    else Just "lightskyblue"

  grayFill =
    if state.lastHover == Just grayId then Just "gray"
    else Just "lightgray"

  blueBorder =
    if state.lastClicked == Just blueId then Just "gold"
    else Nothing

  grayBorder =
    if state.lastClicked == Just grayId then Just "gold"
    else Nothing

colorOverride :: forall cs m. H.ComponentHTML Action cs m
colorOverride =
  HH.div_
    [ HH.h3_ [ HH.text "Color Override" ]
    , ImageSvg.toHalogenSvg ImageSvg.noEvent example
    ]
  where
  example :: Image
  example =
    Image.Style
      Image.defStyle { fill = Just "pink", line = Just "pink" }
      ( interactiveExampleImage
          { lastClicked: Nothing
          , lastHover: Nothing
          }
      )
