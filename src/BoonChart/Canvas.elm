module BoonChart.Canvas exposing (BoonChart, boonChart)

import BoonChart exposing (..)
import Geometry
import Traits exposing (TraitId, Traits, Trait, GodData, God(..), BoonType(..), BoonStatus(..))
import Layout exposing (Layout, GroupId, Boundary(..), Winding(..))
import MouseWheel

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import Json.Decode as Decode
import Set exposing (Set)

type alias Point = (Float, Float)

type alias BoonChart msg =
  { metrics : ChartMetrics
  , activeBasicGroups : List (Set GroupId)
  , activeDuoGroups : Set GroupId
  , boonStatus : Dict TraitId BoonStatus
  , onMouseMove : Point -> msg
  , onMouseDown : Point -> msg
  , onMouseUp : Point -> msg
  , onWheel : Point -> Int -> msg
  , drag : DragMode
  , offset : Point
  , zoom : Float
  , diameter : Float
  , width : Int
  , height : Int
  }

boonChart : List (Html.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes model =
  let
    size = model.diameter * model.zoom
  in
  Canvas.toHtml
    (model.width, model.height)
    (List.append
      attributes
      [ MouseWheel.onWheel model.onWheel
      , when (model.drag == Released) (Events.on "mousedown" (mouseDecoder model.onMouseDown))
      , when (model.drag /= Released) (Events.on "mouseup" (mouseDecoder model.onMouseUp))
      , when (model.drag /= Released) (Events.on "mouseleave" (mouseDecoder model.onMouseUp))
      , when (model.drag /= Released) (Events.on "mousemove" (mouseDecoder model.onMouseMove))
      ] 
    )
    [ clear (0, 0) (model.width |> toFloat) (model.height |> toFloat)
    , shapes [ fill Color.white ] [ circle model.offset 4 ]
    , group
      [ transform
        [ translate (pointX model.offset) (pointY model.offset)
        , scale size size
        , translate 0.5 0.5
        ]
      ]
      [ shapes
        [ stroke Color.white
        , lineWidth 0.01
        ]
        [ circle (0, 0) 0.5 ]
      ]
    ]

when : Bool -> Html.Attribute msg -> Html.Attribute msg
when test att =
  if test then att else Html.Attributes.class ""

pointX : Point -> Float
pointX (x,_) = x

pointY : Point -> Float
pointY (_,y) = y

mouseDecoder tagger =
  (Decode.map tagger clientDecoder)

clientDecoder : Decode.Decoder Point
clientDecoder =
  Decode.map2 Tuple.pair
    (Decode.field "clientX" Decode.float)
    (Decode.field "clientY" Decode.float)
