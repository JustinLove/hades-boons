module BoonChart.Canvas exposing (BoonChart, boonChart)

import BoonChart exposing (..)
import Geometry
import Traits exposing (TraitId, Traits, Trait, GodData, God(..), BoonType(..), BoonStatus(..))
import Layout exposing (Layout, GroupId, Boundary(..), Winding(..))
import MouseWheel

import Array exposing (Array)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced as Canvas
import Canvas.Settings.Line exposing (..)
import Canvas.Settings.Text exposing (..)
import Canvas.Texture as Canvas
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
  , onTexture : String -> Maybe Canvas.Texture -> msg
  , drag : DragMode
  , offset : Point
  , zoom : Float
  , diameter : Float
  , width : Int
  , height : Int
  , textures : Dict String Canvas.Texture
  }

tau = pi*2

boonChart : List (Html.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes model =
  let
    metrics = model.metrics
    displayDiameter = model.diameter * model.zoom
    basicBoons = metrics.gods |> Array.toList |> List.concatMap .boons
    basicConnectors = metrics.gods |> Array.toList |> List.map .connectors
    duoSize = duoBoonSize model.zoom
    basicSize = basicBoonSize model.zoom
  in
  Canvas.toHtmlWith
    { width = model.width
    , height = model.height
    , textures =
      (List.append basicBoons metrics.duoBoons
        |> List.map .icon
        |> (::) "GUI/LockIcon/LockIcon0001.png"
        |> List.map (\icon -> Canvas.loadFromImageUrl icon (model.onTexture icon))
      )
    }
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
        [ translate (flip model.offset)
        , scale displayDiameter
        , translate (0.5, -0.5)
        ]
      ]
      (List.concat
        [ [ shapes
            [ stroke Color.white
            , lineWidth 0.01
            ]
            [ circle (0, 0) 0.5 ]
          ]
        , displayGods metrics
        , basicBoons |> List.map (displayBoonTrait displayDiameter basicSize model.textures model.boonStatus)
        , metrics.duoBoons |> List.map (displayBoonTrait displayDiameter duoSize model.textures model.boonStatus)
        ]
      )
    ]

when : Bool -> Html.Attribute msg -> Html.Attribute msg
when test att =
  if test then att else Html.Attributes.class ""

flip : Point -> Point
flip (x, y) = (x, -y)

displayGods : ChartMetrics -> List Renderable
displayGods metrics =
  metrics.gods
    |> Array.toList
    |> List.map (\godMetrics ->
      displayGod godMetrics
        |> group
          [ transform
            [ translate godMetrics.center
            , rotate godMetrics.angle
            , scale metrics.adjacentDistance
            ]
          ]
    )

displayGod : GodMetrics -> List Renderable
displayGod godMetrics =
  [ shapes
    [ fill (Color.rgba 0.0 0.0 0.0 0.7)
    , stroke (Color.rgb 0.05 0.05 0.05)
    , lineWidth 0.01
    ]
    [ circle (0,0) 0.5 ]
  , text
    [ fill godMetrics.color
    , font { size = 200, family = "sans-serif" }
    , align Center
    , transform [ scale 0.001 ]
    ]
    (0, 0) godMetrics.name
  ]

displayBoonTrait : Float -> Float -> Dict String Canvas.Texture -> Dict TraitId BoonStatus -> Boon -> Renderable
displayBoonTrait displayDiameter size textures boonStatus boon =
  let
    status = Dict.get boon.id boonStatus |> Maybe.withDefault Unavailable
    color =
      case status of
        Active -> Color.white
        Available -> Color.darkGrey
        Excluded -> Color.charcoal
        Unavailable -> Color.charcoal
    brightness =
      case status of
        Active -> 1.0
        Available -> 0.5
        Excluded -> 0.1
        Unavailable -> 0.1
    textLine = \tx sz p ->
      let
        fontSize = size * displayDiameter * sz
        f = (fontSize - 6.0) / 8.0 |> atMost 1.0
        c = darken f color
      in
      if fontSize > 6 then
        [ text
          [ fill c
          , font { size = 100, family = "sans-serif" }
          , align Center
          , transform [ scale (sz * 0.01) ]
          ]
          (0,0) tx
        ]
          |> group [ transform [ translate p ] ]
      else
        group [] []
    side = (0.47 * (sqrt 2))
  in
  [ image textures 0.9 boon.icon
  , shapes
    [ fill (Color.rgba 0 0 0 (1.0 - brightness))
    , transform
      [ translate (-0.47, 0.0)
      , rotate (tau/8)
      ]
    ]
    [ rect (0,0) side side
    ]
  , if status == Excluded then
      image textures 0.7 "GUI/LockIcon/LockIcon0001.png"
    else
      group [] []
  , textLine boon.id 0.1 (0, -0.65)
  , textLine boon.name 0.2 (0, -0.5)
  --, shapes [ fill (Color.white) ] [ circle (0,0) 0.05 ]
  ]
    |> group
      [ transform
        [ translate boon.location
        , scale size
        ]
      ]

image : Dict String Canvas.Texture -> Float -> String -> Renderable
image textures size key =
  Dict.get key textures
    |> Maybe.map (\tex ->
      let {width, height} = Canvas.dimensions tex in
      texture
        [ transform
          [ scale (size/width)
          , translate (-width/2, height/2)
          ]
        ]
        (0,0)
        tex
      )
    |> Maybe.withDefault (group [] [])

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

transform = Canvas.transform
translate (x,y) = Canvas.translate x -y
rotate r = Canvas.rotate -r
scale s = Canvas.scale s s

atLeast : Float -> Float -> Float
atLeast = max

atMost : Float -> Float -> Float
atMost = min

darken : Float -> Color -> Color
darken by color =
  color
    |> Color.toHsla
    |> (\hsla -> {hsla | lightness = hsla.lightness * by})
    |> Color.fromHsla
