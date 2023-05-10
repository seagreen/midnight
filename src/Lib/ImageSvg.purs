module Lib.ImageSvg where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Baseline as Baseline
import Halogen.Svg.Attributes.CSSLength as CssLength
import Halogen.Svg.Attributes.FontSize as FontSize
import Halogen.Svg.Attributes.FontWeight as FontWeight
import Halogen.Svg.Attributes.Path (PathCommand)
import Halogen.Svg.Attributes.Path as Path
import Halogen.Svg.Attributes.TextAnchor as TextAnchor
import Halogen.Svg.Elements as SE
import Lib.Image as Image
import Lib.Image.Internal (Image(..))
import Web.UIEvent.MouseEvent (MouseEvent)

type OnEvent a =
  { onClick :: Maybe (String -> a)
  , onHover :: Maybe (String -> a)
  , onExit :: Maybe (String -> a)
  }

noEvent :: forall a. OnEvent a
noEvent =
  { onClick: Nothing
  , onHover: Nothing
  , onExit: Nothing
  }

toHalogenSvg :: forall a cs m. OnEvent a -> Image -> H.ComponentHTML a cs m
toHalogenSvg onEvent image =
  SE.svg
    [ SA.classes
        [ ClassName "svg-test"
        , ClassName "svg-test--medium"
        ]
    , HP.style ("background-color: white; width: " <> show width <> "; height: " <> show height <> ";")
    , SA.viewBox 0.0 0.0 width height
    ]
    [ placeInCenter (imageToSvg onEvent image)
    ]
  where
  width :: Number
  width =
    850.0

  height :: Number
  height =
    800.0

  -- instead of top left
  placeInCenter :: H.ComponentHTML a cs m -> H.ComponentHTML a cs m
  placeInCenter svg =
    SE.g
      [ SA.transform
          [ SA.Translate (width / 2.0) (height / 2.0)
          ]
      ]
      [ svg
      ]

type Colors =
  { fill :: Maybe SA.Color
  , line :: Maybe SA.Color
  , text :: Maybe SA.Color
  }

defaultFillColor :: SA.Color
defaultFillColor =
  SA.NoColor

defaultLineColor :: SA.Color
defaultLineColor =
  SA.Named "black"

defaultTextColor :: SA.Color
defaultTextColor =
  SA.Named "black"

imageToSvg :: forall a cs m. OnEvent a -> Image -> H.ComponentHTML a cs m
imageToSvg onEvent =
  imageToSvgRec
    { fill: Nothing
    , line: Nothing
    , text: Nothing
    }
    onEvent

imageToSvgRec :: forall a cs m. Colors -> OnEvent a -> Image -> H.ComponentHTML a cs m
imageToSvgRec colors onEvent = case _ of
  Circle mId r ->
    SE.circle
      ( [ SA.r r
        , SA.fill (fromMaybe defaultFillColor colors.fill)
        , SA.stroke (fromMaybe defaultLineColor colors.line)
        ] <> toId mId
      )

  Rectangle mId a ->
    translate
      { x: -(a.width / 2.0)
      , y: a.height / 2.0
      }
      ( SE.rect
          ( [ SA.width a.width
            , SA.height a.height
            , SA.fill (fromMaybe defaultFillColor colors.fill)
            , SA.stroke (fromMaybe defaultLineColor colors.line)
            ] <> toId mId
          )
      )

  Line p1 p2 ->
    SE.line
      [ SA.stroke (fromMaybe defaultLineColor colors.line)
      , SA.x1 p1.x
      , SA.y1 (negate p1.y)
      , SA.x2 p2.x
      , SA.y2 (negate p2.y)
      ]

  Path ps ->
    let
      points :: Array PathCommand
      points =
        case Array.uncons ps of
          Nothing ->
            []
          Just { head } ->
            let
              moveToStart =
                Array.cons (Path.m Path.Abs head.x (negate head.y))
            in
              moveToStart ((\p -> Path.l Path.Abs p.x (negate p.y)) <$> ps)
    in
      -- NOTE: I'd like to represent Path as a SVG polyline, using:
      --
      -- > points :: Array (Tuple Number Number)
      -- > points =
      -- >   (\p -> Tuple p.x p.y) <$> ps
      --
      -- ...but you currently can't use SA.fill with a polyline.
      SE.path
        [ SA.stroke (fromMaybe defaultLineColor colors.line)
        , SA.fill SA.NoColor
        , SA.d points
        ]

  Text s ->
    SE.text
      [ SA.stroke (fromMaybe defaultTextColor colors.text)
      -- , SA.fontSize (FontSize.FontSizeLength (CssLength.Em 2.0))
      , SA.fontFamily "Roboto Mono"
      , SA.fontWeight (FontWeight.FWeightNumber 300.0) -- NOTE: you have to have this exact weight
      -- , SA.fontWeight FontWeight.FWeightBold
      , SA.textAnchor TextAnchor.AnchorStart -- TextAnchor.AnchorMiddle to center
      , SA.dominantBaseline Baseline.BaselineMiddle
      , HP.style "white-space: pre"
      ]
      [ HH.text s ]

  Style style image ->
    imageToSvgRec
      { fill: colors.fill <|> map SA.Named style.fill
      , line: colors.line <|> map SA.Named style.line
      , text: colors.text <|> map SA.Named style.text
      }
      onEvent
      image

  Translate xy image ->
    translate xy (imageToSvgRec colors onEvent image)

  Overlay i1 i2 ->
    SE.g
      []
      [ imageToSvgRec colors onEvent i2
      , imageToSvgRec colors onEvent i1
      ]

  Empty ->
    SE.g [] []
  where
  translate
    :: { x :: Number, y :: Number }
    -> H.ComponentHTML a cs m
    -> H.ComponentHTML a cs m
  translate { x, y } svg =
    SE.g
      [ SA.transform [ SA.Translate x (negate y) ] ]
      [ svg ]

  toId
    :: forall r
     . Maybe Image.Id
    -> Array
         ( HH.IProp
             ( id :: String
             , onClick :: MouseEvent
             , onMouseEnter :: MouseEvent
             , onMouseLeave :: MouseEvent
             | r
             )
             a
         )
  toId mId =
    case mId of
      Nothing ->
        []
      Just (Image.Id id) ->
        Array.catMaybes
          [ Just (SA.id id)
          , (\onClick -> HE.onClick (\_ -> onClick id)) <$> onEvent.onClick
          , (\onHover -> HE.onMouseEnter (\_ -> onHover id)) <$> onEvent.onHover
          , (\onExit -> HE.onMouseLeave (\_ -> onExit id)) <$> onEvent.onExit
          ]
