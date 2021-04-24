module BoonChart exposing (DragMode(..), BoonChart, boonChart)

import Traits exposing (..)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render
import Collage.Text as Text
import Collage.Events as Events
import Color exposing (Color)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Svg
import Svg.Attributes

type DragMode
  = Released
  | Dragging Point Point

type alias BoonChart msg =
  { traits : Traits
  , onMouseMove : Point -> msg
  , onMouseDown : Point -> msg
  , onMouseUp : Point -> msg
  , onWheel : Point -> Int -> msg
  , drag : DragMode
  , offset : Point
  , zoom : Float
  }

size = 500
width = size
height = size

tau = pi*2

godOrder =
  [ "AphroditeUpgrade"
  , "AresUpgrade"
  , "DemeterUpgrade"
  , "DionysusUpgrade"
  , "PoseidonUpgrade"
  , "AthenaUpgrade"
  , "ArtemisUpgrade"
  , "ZeusUpgrade"
  ]

boonChart : List (Svg.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes {traits, onMouseDown, onMouseUp, onMouseMove, onWheel, drag, offset, zoom} =
  [ circle 0.45
      |> outlined (solid 0.01 (uniform Color.white))
  , gods traits
  , rectangle 1 1
      |> filled (uniform Color.black)
      --|> outlined (solid 0.005 (uniform Color.white))
      --|> outlined (solid 0.005 (transparent))
  ]
    |> stack
    |> align topLeft
    |> List.singleton
    |> group
    |> scale size
    |> debug
    |> shift (flip offset)
    |> scale zoom
    |> when (drag == Released) (Events.onMouseDown onMouseDown)
    |> when (drag /= Released) (Events.onMouseUp onMouseUp)
    |> when (drag /= Released) (Events.onMouseLeave onMouseUp)
    |> when (drag /= Released) (Events.onMouseMove onMouseMove)
    |> Collage.Render.svgExplicit ((Html.Events.stopPropagationOn "wheel" (wheelDecoder onWheel) ) :: attributes)

when : Bool -> (Collage msg -> Collage msg) -> Collage msg -> Collage msg
when test f collage =
  if test then f collage else collage

flip : Point -> Point
flip (x, y) = (x, -y)

gods : Traits -> Collage msg
gods traits =
  let
    main = List.drop 1 traits
    count = List.length main
    factor = -tau / (toFloat count)
  in
    main
      |> List.indexedMap (\i data ->
        god data
          |> shiftY 0.35
          |> List.singleton
          |> group
          |> rotate ((toFloat i) * factor)
      )
      |> stack

god : God -> Collage msg
god data =
  Text.fromString data.name
    |> Text.color data.lootColor
    |> Text.size 100
    |> rendered
    |> scale 0.0002

wheelDecoder : (Point -> Int -> msg) -> Decode.Decoder ( msg, Bool )
wheelDecoder tagger =
  Decode.map (\v -> (v, True))
    (Decode.map2 tagger
      clientDecoder
      (Decode.field "deltaY" Decode.int)
    )

clientDecoder : Decode.Decoder Point
clientDecoder =
  Decode.map2 Tuple.pair
    (Decode.field "clientX" Decode.float)
    (Decode.field "clientY" Decode.float)
