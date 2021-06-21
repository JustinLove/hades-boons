module BoonChart.Canvas exposing (BoonChart, boonChart)

import BoonChart exposing (..)
import Geometry
import Traits exposing (TraitId, Traits, Trait, GodData, God(..), BoonType(..), BoonStatus(..))
import Layout exposing (Layout, GroupId, Boundary(..), Winding(..))
import MouseWheel

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
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
  , width : Int
  , height : Int
  }

boonChart : List (Html.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes model =
  let
    size = ((min model.width model.height) |> toFloat) * model.zoom
  in
  Canvas.toHtml
    (model.width, model.height)
    attributes
    [ clear (0, 0) (model.width |> toFloat) (model.height |> toFloat)
    , shapes 
      [ fill Color.white
      , transform
        [ translate (pointX model.offset) (pointY model.offset)
        , scale size size
        , translate 0.5 0.5
        ]
      ]
      [ circle (0, 0) 0.5
        --[ rect (0, 0) (model.width |> toFloat) (model.height |> toFloat)
      ]
    ]

pointX : Point -> Float
pointX (x,_) = x

pointY : Point -> Float
pointY (_,y) = y

