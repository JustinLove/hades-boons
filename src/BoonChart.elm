module BoonChart exposing (DragMode(..), BoonChart, boonChart)

import Geometry
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

type alias Metrics =
  { centers : Array Point
  , angle : Float
  , adjacentDistance : Float
  }

size = 500
width = size
height = size

mainRingRadius = 0.3

tau = pi*2

boonChart : List (Svg.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes {traits, onMouseDown, onMouseUp, onMouseMove, onWheel, drag, offset, zoom} =
  let
    metrics1 = initialMetrics traits
    gods = displayGods metrics1 traits
    metrics2 = {metrics1 | centers = List.map base gods |> Array.fromList}
  in
  --[ circle 0.45
      --|> outlined (solid 0.01 (uniform Color.white))
  [ gods |> stack
  , displayDuos metrics2 (Traits.duoBoons traits)
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

initialMetrics : Traits -> Metrics
initialMetrics traits =
  let
    main = List.drop 1 traits
    count = List.length main
    angle = tau / (toFloat count)
    adjacentDistance = 2 * mainRingRadius * (sin (angle / 2))
  in
    { centers = Array.empty
    , angle = angle
    , adjacentDistance = adjacentDistance
    }

displayGods : Metrics -> Traits -> List (Collage msg)
displayGods metrics traits =
  traits
    |> List.drop 1
    |> List.indexedMap (\i data ->
      displayGod data
        |> scale metrics.adjacentDistance
        |> shiftY mainRingRadius
        |> List.singleton
        |> group
        |> rotate (((toFloat i) * -metrics.angle) + -metrics.angle/2)
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

displayDuos : Metrics -> List Trait -> Collage msg
displayDuos metrics traits =
  traits
    --|> List.filter (isSkipOne metrics)
    --|> List.drop 3
    --|> List.take 1
    |> List.map (displayDuo metrics)
    |> stack

displayDuo : Metrics -> Trait -> Collage msg
displayDuo metrics trait =
  case trait.boonType of
    UnknownBoon -> group []
    BasicBoon _ -> group []
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.centers) a b of
        Adjacent -> displayAdjacent metrics a b
        SkipOne -> displaySkipOne metrics a b
        SkipTwo -> displaySkipTwo metrics a b
        Opposite -> displayOpposite metrics a b

isSkipOne : Metrics -> Trait -> Bool
isSkipOne metrics trait =
  case trait.boonType of
    UnknownBoon -> False
    BasicBoon _ -> False
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.centers) a b of
        Adjacent -> False
        SkipOne -> True
        SkipTwo -> False
        Opposite -> False

displayAdjacent : Metrics -> God -> God -> Collage msg
displayAdjacent metrics a b =
  segment
    (godCenter metrics a)
    (godCenter metrics b)
    |> traced (solid 0.01 (uniform Color.darkGrey))

displaySkipOne : Metrics -> God -> God -> Collage msg
displaySkipOne =
  displayArc Color.red -0.2 (mainRingRadius * 1.45)

displaySkipTwo : Metrics -> God -> God -> Collage msg
displaySkipTwo =
  displayArc Color.blue 0.2 (mainRingRadius * 1.6)

displayArc : Color -> Float -> Float -> Metrics -> God -> God -> Collage msg
displayArc color centerAdjust iconDistance metrics a b =
  let
    centerA = godCenter metrics a
    centerB = godCenter metrics b
    endA = Geometry.interpolate -centerAdjust centerA (0,0)
    endB = Geometry.interpolate -centerAdjust centerB (0,0)
    mid = Geometry.midpoint endA endB
    v = Geometry.normalize mid
    iconPoint = Geometry.scale iconDistance v
    midA = Geometry.midpoint endA iconPoint
    av = Geometry.sub endA midA |> Geometry.normalize
    perp = Geometry.perpendicular av |> Geometry.normalize |> Geometry.scale 0.1
    a2 = Geometry.add midA perp
    center = Geometry.lineIntersection (midA, a2) ((0,0), iconPoint)
    radius = (Geometry.sub iconPoint center) |> Geometry.length
    termA = fartherPoint (center, radius) (centerA, metrics.adjacentDistance/2)
    termB = fartherPoint (center, radius) (centerB, metrics.adjacentDistance/2)
  in
    [ arc center termA iconPoint
      |> traced (solid 0.01 (uniform color))
    , arc center iconPoint termB
      |> traced (solid 0.01 (uniform color))
    , square 0.02
      |> filled (uniform Color.white)
      |> shift iconPoint
    ]
      |> stack

fartherPoint : (Point, Float) -> (Point, Float) -> Point
fartherPoint c1 c2 =
  let
    (p1, p2) = Geometry.circleIntersection c1 c2
  in
    if Geometry.length p1 > Geometry.length p2 then
      p1
    else
      p2

displayOpposite : Metrics -> God -> God -> Collage msg
displayOpposite metrics a b =
  segment
    (godCenter metrics a)
    (godCenter metrics b)
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

godCenter : Metrics -> God -> Point
godCenter metrics who =
  metrics.centers
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
