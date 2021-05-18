module BoonChart exposing (DragMode(..), BoonChart, boonChart)

import Geometry
import Traits exposing (TraitId, Traits, Trait, GodData, God(..), BoonType(..))
import Layout exposing (Layout)

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
import Set exposing (Set)
import Svg
import Svg.Attributes

type DragMode
  = Released
  | Dragging Point Point

type alias BoonChart msg =
  { traits : Traits
  , layout : Layout
  , activeTraits : Set TraitId
  , onMouseMove : Point -> msg
  , onMouseDown : Point -> msg
  , onMouseUp : Point -> msg
  , onWheel : Point -> Int -> msg
  , selectedBoon : TraitId -> msg
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
  = Invisible
  | Arc ArcType
  | DuoArc DuoArcType
  | Line Point Point

type alias ArcType =
  { center : Point
  , radius : Float
  , fromAngle : Float
  , toAngle : Float
  }

type alias DuoArcType =
  { center : Point
  , endA : Point
  , midPoint : Point
  , endB : Point
  }

type alias Boon =
  { name : String
  , icon : String
  , id : String
  , location : Point
  }

size = 4096
width = size
height = size

mainRingRadius = 0.28

tau = pi*2

boonChart : List (Svg.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes model =
  let
    metrics1 = initialMetrics model.traits
    gods = displayGods metrics1 model.traits
    metrics2 = {metrics1 | centers = List.map base gods |> Array.fromList}
    basicBoons = layoutBasicBoons metrics2 model.layout model.traits
    (duoBoons, duoConnectors) = layoutDuoBoons metrics2 (Traits.duoBoons model.traits)
  in
  --[ circle 0.45
      --|> outlined (solid 0.01 (uniform Color.white))
  [ duoBoons |> List.map ((displayBoonTrait model.selectedBoon model.traits model.activeTraits) >> (scale (0.02 / model.zoom |> clamp 0.02 0.08))) |> stack
  , basicBoons |> List.map ((displayBoonTrait model.selectedBoon model.traits model.activeTraits) >> (scale (0.02 / model.zoom |> clamp 0.01 0.02))) |> stack
  , duoConnectors |> List.map displayBoonConnector |> stack
  , layoutBasicConnectors metrics2 model.layout |> List.map displayBoonConnector |> stack
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
    |> shift (flip model.offset)
    |> scale model.zoom
    |> when (model.drag == Released) (Events.onMouseDown model.onMouseDown)
    |> when (model.drag /= Released) (Events.onMouseUp model.onMouseUp)
    |> when (model.drag /= Released) (Events.onMouseLeave model.onMouseUp)
    |> when (model.drag /= Released) (Events.onMouseMove model.onMouseMove)
    |> Collage.Render.svgExplicit ((Html.Events.stopPropagationOn "wheel" (wheelDecoder model.onWheel) ) :: attributes)

when : Bool -> (Collage msg -> Collage msg) -> Collage msg -> Collage msg
when test f collage =
  if test then f collage else collage

flip : Point -> Point
flip (x, y) = (x, -y)

initialMetrics : Traits -> ChartMetrics
initialMetrics traits =
  let
    main = Traits.linkableGods traits
    count = List.length main
    angle = tau / (toFloat count)
    adjacentDistance = 2 * mainRingRadius * (sin (angle / 2))
  in
    { centers = Array.empty
    , angle = angle
    , adjacentDistance = adjacentDistance
    }

ringLayout : Int -> Float -> ChartMetrics
ringLayout count radius =
  let
    angle = tau / (toFloat count)
    adjacentDistance = 2 * mainRingRadius * (sin (angle / 2))
  in
    { centers = List.range 0 (count - 1)
      |> List.map (\i ->
        (0, radius)
          |> Geometry.rotate (((toFloat i) * -angle) + -angle/2)
      )
      |> Array.fromList
    , angle = angle
    , adjacentDistance = adjacentDistance
    }

displayGods : ChartMetrics -> Traits -> List (Collage msg)
displayGods metrics traits =
  traits
    |> Traits.linkableGods
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
  [ Text.fromString (Traits.dataName data)
      |> Text.color (Traits.dataLootColor data)
      |> Text.size 200
      |> rendered
      |> scale 0.001
  , circle 0.5
      |> outlined (solid 0.01 (uniform Color.white))
  ]
    |> stack
    |> Collage.Layout.name (Traits.dataName data)

layoutBasicConnectors : ChartMetrics -> Layout -> List ConnectorShape
layoutBasicConnectors metrics layout =
  let
    origin = (godCenter metrics Demeter)
    scaleFactor = 1/1800
    toScale = Geometry.scale scaleFactor >> Geometry.add origin
  in
    layout.connections
      |> List.map (\{shape} ->
        case shape of
          Layout.Line a b ->
            Line (a |> toScale) (b |> toScale)
          Layout.Arc {center, radius, fromAngle, toAngle} ->
            Arc
              { center = center |> toScale
              , radius = radius * scaleFactor
              , fromAngle = fromAngle
              , toAngle = toAngle
              }
      )

layoutBasicBoons : ChartMetrics -> Layout -> Traits -> List Boon
layoutBasicBoons metrics layout traits =
  traits
    |> Traits.allGods
    |> List.concatMap (layoutBasicBoonsOf metrics layout)

layoutBasicBoonsOf : ChartMetrics -> Layout -> GodData -> List Boon
layoutBasicBoonsOf metrics layout data =
  let
    center = (godCenter metrics (Traits.dataGod data))
    boons = Traits.basicBoons data
    angle = tau / (toFloat (List.length boons))
    adjacentDistance = 2 * mainRingRadius * (sin (angle / 2))
  in
    boons
      |> List.indexedMap (\i trait ->
        let
          p = (case Layout.getPlacement layout trait.trait of
            Just (x,y) -> (x / 1800, y / 1800)
            Nothing -> (0, 0.05)
              |> Geometry.rotate (((toFloat i) * -angle) + -angle/2)
            )
            |> Geometry.add center
        in
          { name = trait.name
          , icon = trait.icon
          , id = trait.trait
          , location = p
          }
      )

layoutDuoBoons : ChartMetrics -> List Trait -> (List Boon, List ConnectorShape)
layoutDuoBoons metrics traits =
  traits
    --|> List.filter (isSkipOne metrics)
    --|> List.drop 3
    --|> List.take 1
    |> List.map (layoutDuoBoon metrics)
    |> List.unzip

layoutDuoBoon : ChartMetrics -> Trait -> (Boon, ConnectorShape)
layoutDuoBoon metrics trait =
  let
    (iconPoint, shape) = calculateDuo metrics trait
  in
    ( { name = trait.name
      , icon = trait.icon
      , id = trait.trait
      , location = iconPoint
      }
    , shape
    )

isSkipOne : ChartMetrics -> Trait -> Bool
isSkipOne metrics trait =
  case trait.boonType of
    BasicBoon _ -> False
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.centers) a b of
        Adjacent -> False
        SkipOne -> True
        SkipTwo -> False
        Opposite -> False

displayBoonTrait : (TraitId -> msg) -> Traits ->Set TraitId -> Boon -> Collage msg
displayBoonTrait selectedBoon traits activeTraits boon =
  let
    avail = Traits.isAvailable traits activeTraits boon.id
    color =
      if Set.member boon.id activeTraits then
        Color.white
      else if avail then
        Color.darkGrey
      else
        Color.charcoal
    brightness =
      if Set.member boon.id activeTraits then
        1.0
      else if avail then
        0.5
      else
        0.1
  in
  [ Text.fromString boon.name
      |> Text.color color
      |> Text.size 200
      |> rendered
      |> scale 0.001
      |> shiftY -0.5
  , Text.fromString boon.id
      |> Text.color color
      |> Text.size 100
      |> rendered
      |> scale 0.001
      |> shiftY -0.65
  , square (0.47 * (sqrt 2))
    |> filled (uniform Color.black)
    |> opacity (1.0 - brightness)
    |> rotate (tau/8)
  , image (0.9, 0.9) boon.icon
  ]
    |> stack
    |> shift boon.location
    |> Events.onClick (selectedBoon boon.id)

displayBoonConnector : ConnectorShape -> Collage msg
displayBoonConnector shape =
  let
    lineStyle = solid 0.004 (uniform Color.charcoal)
  in
  case shape of
    Invisible ->
      group []
    Arc arcInfo ->
      arcFromAngles arcInfo
        |> traced lineStyle
    DuoArc arc ->
      [ arcFromPoints arc.center arc.endA arc.midPoint
        |> traced lineStyle
      , arcFromPoints arc.center arc.midPoint arc.endB
        |> traced lineStyle
      ]
        |> stack
    Line a b->
      segment a b
        |> traced lineStyle

calculateDuo : ChartMetrics -> Trait -> (Point, ConnectorShape)
calculateDuo metrics trait =
  case trait.boonType of
    BasicBoon _ -> ((0,0), Invisible)
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.centers) a b of
        Adjacent -> calculateAdjacent metrics a b
        SkipOne -> calculateSkipOne metrics a b
        SkipTwo -> calculateSkipTwo metrics a b
        Opposite -> calculateOpposite metrics a b

calculateAdjacent : ChartMetrics -> God -> God -> (Point, ConnectorShape)
calculateAdjacent metrics a b =
  let
    endA = godCenter metrics a
    endB = godCenter metrics b
    iconPoint = Geometry.midpoint endA endB
  in
    (iconPoint, Invisible)

calculateSkipOne : ChartMetrics -> God -> God -> (Point, ConnectorShape)
calculateSkipOne =
  calculateArc -0.2 (mainRingRadius * 1.45)

calculateSkipTwo : ChartMetrics -> God -> God -> (Point, ConnectorShape)
calculateSkipTwo =
  calculateArc 0.2 (mainRingRadius * 1.6)

calculateOpposite : ChartMetrics -> God -> God -> (Point, ConnectorShape)
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
    (iconPoint, (Line endA endB))

calculateArc : Float -> Float -> ChartMetrics -> God -> God -> (Point, ConnectorShape)
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
    ( iconPoint
    , DuoArc
        { center = center
        , endA = termA
        , midPoint = iconPoint
        , endB = termB
        }
    )

fartherPoint : (Point, Float) -> (Point, Float) -> Point
fartherPoint c1 c2 =
  let
    (p1, p2) = Geometry.circleIntersection c1 c2
  in
    if Geometry.length p1 > Geometry.length p2 then
      p1
    else
      p2

arcPoints : Point -> Point -> Point -> List Point
arcPoints (xc,yc) (x1,y1) (x4,y4) =
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
  [(x1,y1), (x2,y2), (x3,y3), (x4,y4)]

arcFromPoints : Point -> Point -> Point -> Path
arcFromPoints c p1 p2 =
  cubicCurve (arcPoints c p1 p2)

arcFromAngles : ArcType -> Path
arcFromAngles {center, radius, fromAngle, toAngle} =
  let
    oddAngle = abs (toAngle - fromAngle)
    angle =
      if oddAngle > pi then
        tau - oddAngle
      else
        oddAngle
    workAngle = oddAngle / 2 + fromAngle
    midAngle =
      if workAngle > tau then
        workAngle - tau
      else
        workAngle
    midPoint = center |> Geometry.add (Geometry.rotate midAngle (radius, 0))
    endA = center |> Geometry.add (Geometry.rotate fromAngle (radius, 0))
    endB = center |> Geometry.add (Geometry.rotate toAngle (radius, 0))
  in
    if angle > tau/4 then
      List.append
        (arcPoints center endA midPoint)
        (List.drop 1 (arcPoints center midPoint endB))
        |> cubicCurve
    else
      (arcPoints center endA endB)
        |> cubicCurve

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

atLeast : Float -> Float -> Float
atLeast = max

atMost : Float -> Float -> Float
atMost = min

clamp : Float -> Float -> Float -> Float
clamp lower upper x =
  x
    |> atLeast lower
    |> atMost upper
