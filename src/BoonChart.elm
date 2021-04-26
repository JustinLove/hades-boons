module BoonChart exposing (DragMode(..), BoonChart, boonChart)

import Traits exposing (..)

import Array exposing (Array)
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

boonChart : List (Svg.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes {traits, onMouseDown, onMouseUp, onMouseMove, onWheel, drag, offset, zoom} =
  let
    gods = displayGods traits
    centers = List.map base gods |> Array.fromList
  in
  [ circle 0.45
      |> outlined (solid 0.01 (uniform Color.white))
  , gods |> stack
  , displayDuos centers (Traits.duoBoons traits)
  , rectangle 1 1
      --|> filled (uniform Color.black)
      |> styled (uniform Color.black, dot 0.001 (uniform Color.white))
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

displayGods : Traits -> List (Collage msg)
displayGods traits =
  let
    main = List.drop 1 traits
    count = List.length main
    factor = tau / (toFloat count)
    radius = 0.25
    adjacentDistance = 2 * radius * (sin (factor / 2))
  in
    main
      |> List.indexedMap (\i data ->
        displayGod data
          |> scale adjacentDistance
          |> shiftY radius
          |> List.singleton
          |> group
          |> rotate ((toFloat i) * -factor)
      )

displayGod : GodData -> Collage msg
displayGod data =
  [ Text.fromString (Traits.godName data.god)
      |> Text.color data.lootColor
      |> Text.size 200
      |> rendered
      |> scale 0.001
  , circle 0.5
      |> outlined (solid 0.01 (uniform Color.white))
  ]
    |> stack

displayDuos : Array Point -> List Trait -> Collage msg
displayDuos centers traits =
  traits
    |> List.map (displayDuo centers)
    |> stack

displayDuo : Array Point -> Trait -> Collage msg
displayDuo centers trait =
  case trait.boonType of
    UnknownBoon -> group []
    BasicBoon _ -> group []
    DuoBoon a b ->
      case godAdjacency (Array.length centers) a b of
        Adjacent -> displayAdjacent centers a b
        SkipOne -> displaySkipOne centers a b
        SkipTwo -> displaySkipTwo centers a b
        Opposite -> displayOpposite centers a b

displayAdjacent : Array Point -> God -> God -> Collage msg
displayAdjacent centers a b =
  segment
    (godCenter centers a)
    (godCenter centers b)
    |> traced (solid 0.01 (uniform Color.darkGrey))

displaySkipOne : Array Point -> God -> God -> Collage msg
displaySkipOne centers a b =
  let
    centerA = (godCenter centers a)
    centerB = (godCenter centers b)
    mid = midpoint centerA centerB
    tweak = interpolate 0.01 mid (0,0)
  in
  arc tweak centerA centerB
    |> traced (solid 0.01 (uniform Color.red))

displaySkipTwo : Array Point -> God -> God -> Collage msg
displaySkipTwo centers a b =
  let
    centerA = (godCenter centers a)
    centerB = (godCenter centers b)
    mid = midpoint centerA centerB
    tweak = interpolate 0.01 mid (0,0)
  in
  arc tweak centerA centerB
    |> traced (solid 0.01 (uniform Color.blue))

displayOpposite : Array Point -> God -> God -> Collage msg
displayOpposite centers a b =
  segment
    (godCenter centers a)
    (godCenter centers b)
    |> traced (solid 0.01 (uniform Color.white))

arc : Point -> Point -> Point -> Path
arc (xc,yc) (x1,y1) (x4,y4) =
  let
    -- https://stackoverflow.com/a/44829356/30203
    ax = x1 - xc
    ay = y1 - yc
    bx = x4 - xc
    by = y4 - yc
    q1 = ax * ax + ay * ay
    q2 = q1 + ax * bx + ay * by
    k2 = (4/3) * ((sqrt (2 * q1 * q2)) - q2) / (ax * by - ay * bx)

    x2 = xc + ax - k2 * ay
    y2 = yc + ay + k2 * ax
    x3 = xc + bx + k2 * by
    y3 = yc + by - k2 * bx
  in
  cubicCurve [(x1,y1), (x2,y2), (x3,y3), (x4,y4)]

godCenter : Array Point -> God -> Point
godCenter centers who =
  centers
    |> Array.get (godIndex who)
    |> Maybe.withDefault (0,0)

type Adjacency
  = Adjacent
  | SkipOne
  | SkipTwo
  | Opposite

godAdjacency : Int -> God -> God -> Adjacency
godAdjacency count a b =
  case abs ((godIndex a) - (godIndex b)) of
    1 -> Adjacent
    2 -> SkipOne
    3 -> SkipTwo
    4 -> Opposite
    5 -> SkipTwo
    6 -> SkipOne
    7 -> Adjacent
    _ -> Opposite

godIndex : God -> Int
godIndex who =
  case who of
    Hermes -> 8
    Aphrodite -> 0
    Ares -> 1
    Demeter -> 2
    Dionysus -> 3
    Poseidon -> 4
    Athena -> 5
    Artemis -> 6
    Zeus -> 7

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

midpoint : Point -> Point -> Point
midpoint (xa,ya) (xb,yb) =
  (xa + (xb - xa)/2, ya + ((yb - ya)/2))

interpolate : Float -> Point -> Point -> Point
interpolate factor (xa,ya) (xb,yb) =
  (xa + (xb - xa)*factor, ya + ((yb - ya)*factor))
