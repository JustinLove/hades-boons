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

type alias ChartMetrics =
  { centers : Array Point
  , angle : Float
  , adjacentDistance : Float
  }

type ConnectorShape
  = Arc Point
  | Line
  | Point

type alias Connector =
  { iconPoint : Point
  , endA : Point
  , endB : Point
  , shape : ConnectorShape
  }

type alias Boon =
  { name : String
  , icon : String
  , iconPoint : Point
  , endA : Point
  , endB : Point
  , shape : ConnectorShape
  }

size = 500
width = size
height = size

mainRingRadius = 0.28

tau = pi*2

boonChart : List (Svg.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes {traits, onMouseDown, onMouseUp, onMouseMove, onWheel, drag, offset, zoom} =
  let
    metrics1 = initialMetrics traits
    gods = displayGods metrics1 traits
    metrics2 = {metrics1 | centers = List.map base gods |> Array.fromList}
    boons = layoutBoons metrics2 (Traits.duoBoons traits)
  in
  --[ circle 0.45
      --|> outlined (solid 0.01 (uniform Color.white))
  [ boons |> List.map displayBoonTrait |> stack
  , boons |> List.map displayBoonConnector |> stack
  , gods |> stack
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

initialMetrics : Traits -> ChartMetrics
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

displayGods : ChartMetrics -> Traits -> List (Collage msg)
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

layoutBoons : ChartMetrics -> List Trait -> List Boon
layoutBoons metrics traits =
  traits
    --|> List.filter (isSkipOne metrics)
    --|> List.drop 3
    --|> List.take 1
    |> List.map (layoutBoon metrics)

layoutBoon : ChartMetrics -> Trait -> Boon
layoutBoon metrics trait =
  let
    {iconPoint, endA, endB, shape} = calculateDuo metrics trait
  in
    { name = trait.name
    , icon = trait.icon
    , iconPoint = iconPoint
    , endA = endA
    , endB = endB
    , shape = shape
    }

isSkipOne : ChartMetrics -> Trait -> Bool
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

displayBoonTrait : Boon -> Collage msg
displayBoonTrait boon =
  [ Text.fromString boon.name
      |> Text.color Color.white
      |> Text.size 200
      |> rendered
      |> scale 0.001
      |> shiftY -0.5
  , image (0.9,0.9) boon.icon
  --, circle 0.5
      --|> outlined (solid 0.01 (uniform Color.white))
  ]
    |> stack
    |> scale 0.08
    |> shift boon.iconPoint

displayBoonConnector : Boon -> Collage msg
displayBoonConnector {iconPoint, endA, endB, shape} =
  let
    lineStyle = solid 0.004 (uniform Color.charcoal)
  in
  case shape of
    Arc center ->
      [ arc center endA iconPoint
        |> traced lineStyle
      , arc center iconPoint endB
        |> traced lineStyle
      ]
        |> stack
    Line ->
      segment
        endA
        endB
        |> traced lineStyle
    Point ->
      group []

displayOpposite : ChartMetrics -> God -> God -> Collage msg
displayOpposite metrics a b =
  segment
    (godCenter metrics a)
    (godCenter metrics b)
    |> traced (solid 0.01 (uniform Color.white))

calculateDuos : ChartMetrics -> List Trait -> List Connector
calculateDuos metrics traits =
  traits
    --|> List.filter (isSkipOne metrics)
    --|> List.drop 3
    --|> List.take 1
    |> List.map (calculateDuo metrics)

calculateDuo : ChartMetrics -> Trait -> Connector
calculateDuo metrics trait =
  case trait.boonType of
    UnknownBoon -> Connector (0,0) (0,0) (0,0) Line
    BasicBoon _ -> Connector (0,0) (0,0) (0,0) Line
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.centers) a b of
        Adjacent -> calculateAdjacent metrics a b
        SkipOne -> calculateSkipOne metrics a b
        SkipTwo -> calculateSkipTwo metrics a b
        Opposite -> calculateOpposite metrics a b

calculateAdjacent : ChartMetrics -> God -> God -> Connector
calculateAdjacent metrics a b =
  let
    endA = godCenter metrics a
    endB = godCenter metrics b
    iconPoint = Geometry.midpoint endA endB
  in
    Connector iconPoint iconPoint iconPoint Point

calculateSkipOne : ChartMetrics -> God -> God -> Connector
calculateSkipOne =
  calculateArc -0.2 (mainRingRadius * 1.45)

calculateSkipTwo : ChartMetrics -> God -> God -> Connector
calculateSkipTwo =
  calculateArc 0.2 (mainRingRadius * 1.6)

calculateOpposite : ChartMetrics -> God -> God -> Connector
calculateOpposite metrics a b =
  let
    length = mainRingRadius - metrics.adjacentDistance/2
    endA = godCenter metrics a
      |> Geometry.normalize
      |> Geometry.scale length
    endB = godCenter metrics b
      |> Geometry.normalize
      |> Geometry.scale length
    iconPoint =
      if (pointY endA) < (pointY endB) then
        Geometry.interpolate 0.2 endB endA
      else
        Geometry.interpolate 0.2 endA endB
  in
    Connector iconPoint endA endB Line

calculateArc : Float -> Float -> ChartMetrics -> God -> God -> Connector
calculateArc centerAdjust iconDistance metrics a b =
  let
    -- where the center of the god's area is
    centerA = godCenter metrics a
    centerB = godCenter metrics b
    -- tweak the target point to change the radius of the arc
    endA = Geometry.interpolate -centerAdjust centerA (0,0)
    endB = Geometry.interpolate -centerAdjust centerB (0,0)
    -- we need to break up the arc because our bezier curve method doesn't work well with angles over 90deg. Conveniently, the middle is also where we want to put the boon.
    mid = Geometry.midpoint endA endB
    v = Geometry.normalize mid
    iconPoint = Geometry.scale iconDistance v
    -- we need to calculate the radius of the arcs circle.
    --  This can be done from the intersection of two lines drawn from the midpoints of lines connecting two points each on the circle
    -- We already have one such line from calculating the icon point
    -- Get one more between icon point and one of the ends
    midA = Geometry.midpoint endA iconPoint
    av = Geometry.sub endA midA |> Geometry.normalize
    perp = Geometry.perpendicular av |> Geometry.normalize |> Geometry.scale 0.1
    a2 = Geometry.add midA perp
    -- center of the circle which our arc is part of
    center = Geometry.lineIntersection (midA, a2) ((0,0), iconPoint)
    -- now we can figure out the radius thereof
    radius = (Geometry.sub iconPoint center) |> Geometry.length
    -- terminal points of the arcs are at one of the intersections between the two circles
    termA = fartherPoint (center, radius) (centerA, metrics.adjacentDistance/2)
    termB = fartherPoint (center, radius) (centerB, metrics.adjacentDistance/2)
  in
    Connector iconPoint termA termB (Arc center)

fartherPoint : (Point, Float) -> (Point, Float) -> Point
fartherPoint c1 c2 =
  let
    (p1, p2) = Geometry.circleIntersection c1 c2
  in
    if Geometry.length p1 > Geometry.length p2 then
      p1
    else
      p2

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

godCenter : ChartMetrics -> God -> Point
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

pointX : Point -> Float
pointX (x,_) = x

pointY : Point -> Float
pointY (_,y) = y
