module BoonChart exposing (DragMode(..), BoonChart, boonChart, hitChart, ChartMetrics, calculateMetrics)

import Geometry
import Traits exposing (TraitId, Traits, Trait, GodData, God(..), BoonType(..), BoonStatus(..))
import Layout exposing (Layout, GroupId)

import Array exposing (Array)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render
import Collage.Text as Text
import Collage.Events as Events
import Color exposing (Color)
import Dict exposing (Dict)
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
  }

type alias ChartMetrics =
  { gods : Array GodMetrics
  , duoBoons : List Boon
  , duoConnectors : List DuoConnector
  , angle : Float
  , adjacentDistance : Float
  }

type alias GodMetrics =
  { center : Point
  , angle : Float
  , god : God
  , name : String
  , color : Color
  , boons : List Boon
  , connectors : List Connector
  }

type alias Connector =
  { shape : ConnectorShape
  , link : Maybe TraitId
  , group : String
  }

type ConnectorShape
  = Arc ArcType
  | Circle Point Float
  | Line Point Point
  | PolyLine (List Point)

type alias ArcType =
  { center : Point
  , radius : Float
  , fromAngle : Float
  , toAngle : Float
  }

type alias DuoConnector =
  { shape : DuoConnectorShape
  , groupA : GroupId
  , colorA : Color
  , groupB : GroupId
  , colorB : Color
  }

type DuoConnectorShape
  = Invisible
  | DuoArc DuoArcType
  | DuoLine Point Point

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
    metrics = model.metrics
    basicBoons = metrics.gods |> Array.toList |> List.concatMap .boons
    basicConnectors = metrics.gods |> Array.toList |> List.map .connectors
  in
  --[ circle 0.45
      --|> outlined (solid 0.01 (uniform Color.white))
  [ metrics.duoBoons |> List.map ((displayBoonTrait model.boonStatus) >> (scale (duoBoonSize model.zoom))) |> stack
  , basicBoons |> List.map ((displayBoonTrait model.boonStatus) >> (scale (basicBoonSize model.zoom))) |> stack
  , metrics.duoConnectors |> List.map (displayDuoConnector model.activeDuoGroups) |> stack
  , List.map2 (\active cons -> List.map (displayBoonConnector model.boonStatus active) cons |> stack)
      model.activeBasicGroups
      basicConnectors
      |> stack
  , displayGods metrics |> stack
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

hitChart : ChartMetrics -> Point -> Float -> Point -> Maybe TraitId
hitChart metrics offset zoom at =
  let
    point = at 
      |> Geometry.add (Geometry.scale -1 offset)
      |> Geometry.scale (1/size)
      |> Geometry.scale (1/zoom)
      |> Geometry.add (-0.5,-0.5)
      |> flip
    godHit = metrics.gods
      |> Array.toList
      |> List.filterMap (hitGod (metrics.adjacentDistance / 2) ((basicBoonSize zoom) / 2) point)
      |> List.head
  in
    case godHit of
      Just _ -> godHit
      Nothing -> hitBoons ((duoBoonSize zoom) / 2) point metrics.duoBoons

hitBoons : Float -> Point -> List Boon -> Maybe TraitId
hitBoons radius at boons =
  boons
    |> List.filter (hitBoon radius at)
    |> List.head
    |> Maybe.map .id

hitBoon : Float -> Point -> Boon -> Bool
hitBoon radius at {location} =
  Geometry.length (Geometry.sub at location) < radius

hitGod : Float -> Float -> Point -> GodMetrics -> Maybe TraitId
hitGod godRadius boonRadius at godMetrics =
  if Geometry.length (Geometry.sub at godMetrics.center) < godRadius then
    godMetrics.boons
      |> hitBoons boonRadius at
  else
    Nothing

duoBoonSize : Float -> Float
duoBoonSize zoom = (0.02 / zoom |> clamp 0.02 0.08)

basicBoonSize : Float -> Float
basicBoonSize zoom = (0.02 / zoom |> clamp 0.01 0.02)

when : Bool -> (Collage msg -> Collage msg) -> Collage msg -> Collage msg
when test f collage =
  if test then f collage else collage

flip : Point -> Point
flip (x, y) = (x, -y)

calculateMetrics : Traits -> ChartMetrics
calculateMetrics traits =
  let
    metrics = initialMetrics traits
    (duoBoons, duoConnectors) = layoutDuoBoons metrics (Traits.duoBoons traits)
  in
    { metrics
    | duoBoons = duoBoons
    , duoConnectors = duoConnectors
    }

initialMetrics : Traits -> ChartMetrics
initialMetrics traits =
  let
    main = Traits.linkableGods traits
    count = List.length main
    angle = tau / (toFloat count)
    adjacentDistance = 2 * mainRingRadius * (sin (angle / 2))
  in
    { gods = Traits.allGods traits
      |> List.sortBy (Traits.dataGod >> godIndex)
      |> List.indexedMap (\i data ->
        let
          a =
            if i == 0 then 0
            else (((toFloat (i-1)) * -angle) + -angle/2)
          center =
            if i == 0 then (0,0)
            else (0, mainRingRadius) |> Geometry.rotate a
        in
          { center = center
          , angle = a
          , god = Traits.dataGod data
          , name = Traits.dataName data
          , color = Traits.dataLootColor data
          , boons = layoutBasicBoonsOf (adjacentDistance/2) center a data
          , connectors = layoutBasicConnectorsOf (adjacentDistance/2) center a data
          }
      )
      |> Array.fromList
    , duoBoons = []
    , duoConnectors = []
    , angle = angle
    , adjacentDistance = adjacentDistance
    }

displayGods : ChartMetrics -> List (Collage msg)
displayGods metrics =
  metrics.gods
    |> Array.toList
    |> List.map (\godMetrics ->
      displayGod godMetrics
        |> scale metrics.adjacentDistance
        |> rotate (godMetrics.angle)
        |> shift (godMetrics.center)
    )

displayGod : GodMetrics -> Collage msg
displayGod godMetrics =
  [ Text.fromString (godMetrics.name)
      |> Text.color (godMetrics.color)
      |> Text.size 200
      |> rendered
      |> scale 0.001
  , circle 0.5
      |> outlined (solid 0.01 (uniform Color.white))
  ]
    |> stack
    |> Collage.Layout.name (godMetrics.name)

layoutBasicConnectorsOf : Float -> Point -> Float -> GodData -> List Connector
layoutBasicConnectorsOf godRadius origin godAngle data =
  let
    layout = Traits.dataLayout data
    scaleFactor = godRadius/layout.radius
    toScale = Geometry.scale scaleFactor >> Geometry.rotate godAngle >> Geometry.add origin
  in
    data
      |> Traits.dataLayout
      |> .connections
      |> List.map (\{group, link, shape} ->
        case shape of
          Layout.Arc {center, radius, fromAngle, toAngle} ->
            { shape = Arc
              { center = center |> toScale
              , radius = radius * scaleFactor
              , fromAngle = fromAngle + godAngle
              , toAngle = toAngle + godAngle
              }
            , link = link
            , group = group
            }
          Layout.Circle center radius ->
            { shape = Circle
              (center |> toScale)
              (radius * scaleFactor)
            , link = link
            , group = group
            }
          Layout.Line a b ->
            { shape = Line (a |> toScale) (b |> toScale)
            , link = link
            , group = group
            }
          Layout.PolyLine points ->
            { shape = PolyLine (points |> List.map toScale)
            , link = link
            , group = group
            }
          Layout.Area shapes ->
            { shape = PolyLine (shapes |> List.map mapAreaSegments |> List.map toScale)
            , link = link
            , group = group
            }
      )

mapAreaSegments : Layout.ConnectionType -> Point
mapAreaSegments shape =
  case shape of
    Layout.Arc {center, radius, fromAngle, toAngle} ->
      center
    Layout.Circle center radius ->
      center
    Layout.Line a b ->
      a
    Layout.PolyLine points ->
      List.head points
        |> Maybe.withDefault (0,0)
    Layout.Area shapes ->
      (0,0)

layoutBasicBoonsOf : Float -> Point -> Float -> GodData -> List Boon
layoutBasicBoonsOf godRadius center godAngle data =
  let
    layout = Traits.dataLayout data
    scaleFactor = godRadius/layout.radius
    boons = Traits.basicBoons data
    angle = tau / (toFloat (List.length boons))
  in
    boons
      |> List.indexedMap (\i trait ->
        let
          p = (case Layout.getPlacement layout trait.trait of
            Just b -> Geometry.scale scaleFactor b
            Nothing -> (0, 0.05)
              |> Geometry.rotate (((toFloat i) * -angle) + -angle/2)
            )
            |> Geometry.rotate godAngle
            |> Geometry.add center
        in
          { name = trait.name
          , icon = trait.icon
          , id = trait.trait
          , location = p
          }
      )

layoutDuoBoons : ChartMetrics -> List Trait -> (List Boon, List DuoConnector)
layoutDuoBoons metrics traits =
  traits
    --|> List.filter (isSkipOne metrics)
    --|> List.drop 3
    --|> List.take 1
    |> List.map (layoutDuoBoon metrics)
    |> List.unzip

layoutDuoBoon : ChartMetrics -> Trait -> (Boon, DuoConnector)
layoutDuoBoon metrics trait =
  let
    (iconPoint, shape) = calculateDuo metrics trait
    (godA, godB) = case trait.boonType of
      BasicBoon g -> (g, g)
      DuoBoon a b -> (a, b)
  in
    ( { name = trait.name
      , icon = trait.icon
      , id = trait.trait
      , location = iconPoint
      }
    , { shape = shape
      , groupA = (Traits.godName godA) ++ trait.trait
      , colorA = colorForGod metrics godA
      , groupB = (Traits.godName godB) ++ trait.trait
      , colorB = colorForGod metrics godB
      }
    )

isSkipOne : ChartMetrics -> Trait -> Bool
isSkipOne metrics trait =
  case trait.boonType of
    BasicBoon _ -> False
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.gods) a b of
        Adjacent -> False
        SkipOne -> True
        SkipTwo -> False
        Opposite -> False

displayBoonTrait : Dict TraitId BoonStatus -> Boon -> Collage msg
displayBoonTrait boonStatus boon =
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
  , if status == Excluded then
      image (0.9, 0.9) "GUI/LockIcon/LockIcon0001.png"
    else
      group []
  , square (0.47 * (sqrt 2))
    |> filled (uniform Color.black)
    |> opacity (1.0 - brightness)
    |> rotate (tau/8)
  , image (0.9, 0.9) boon.icon
  ]
    |> stack
    |> shift boon.location

displayBoonConnector : Dict TraitId BoonStatus -> Set GroupId -> Connector -> Collage msg
displayBoonConnector boonStatus activeGroups {shape, link, group} =
  let
    (thickness, color) =
      if Set.member group activeGroups then
        case link of
          Just id ->
            if Dict.get id boonStatus == Just Active || Set.member id activeGroups then
              (0.004, (uniform Color.white))
            else
              (0.001, (uniform Color.charcoal))
          Nothing ->
            (0.004, (uniform Color.white))
      else
        case link of
          Just id ->
            (0.001, (uniform Color.charcoal))
          Nothing ->
            (0.002, (uniform Color.charcoal))
    lineStyle = solid thickness color
  in
  case shape of
    Arc arcInfo ->
      arcFromAngles arcInfo
        |> traced lineStyle
    Circle center radius ->
      circle radius
        |> outlined lineStyle
        |> shift center
    Line a b ->
      segment a b
        |> traced lineStyle
    PolyLine points ->
      path points
        |> traced lineStyle

displayDuoConnector : Set GroupId -> DuoConnector -> Collage msg
displayDuoConnector activeGroups {shape, groupA, colorA, groupB, colorB} =
  let
    lineStyleA = duoDash False activeGroups groupA colorA
    lineStyleB = duoDash True activeGroups groupB colorB
  in
  case shape of
    Invisible ->
      Collage.group []
    DuoArc arc ->
      let
        arcA = arcFromPoints arc.center arc.endA arc.midPoint
        arcB = arcFromPoints arc.center arc.midPoint arc.endB
      in
      [ arcA
        |> traced lineStyleA
      , arcB
        |> traced lineStyleA
      , arcA
        |> traced lineStyleB
      , arcB
        |> traced lineStyleB
      ]
        |> stack
    DuoLine a b->
      [ segment a b
        |> traced lineStyleA
      , segment a b
        |> traced lineStyleB
      ]
        |> stack

duoDash : Bool -> Set GroupId -> GroupId -> Color -> LineStyle
duoDash which activeGroups group godColor =
  let
    pattern =
      if which then
        [ (0.01, 0), (0, 0.01) ]
      else
        [ (0, 0.01), (0.01, 0) ]
    (thickness, color) =
      if Set.member group activeGroups then
        (0.004, (uniform godColor))
      else
        (0.002, (uniform Color.charcoal))
  in
    broken pattern thickness color

calculateDuo : ChartMetrics -> Trait -> (Point, DuoConnectorShape)
calculateDuo metrics trait =
  case trait.boonType of
    BasicBoon _ -> ((0,0), Invisible)
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.gods) a b of
        Adjacent -> calculateAdjacent metrics a b
        SkipOne -> calculateSkipOne metrics a b
        SkipTwo -> calculateSkipTwo metrics a b
        Opposite -> calculateOpposite metrics a b

calculateAdjacent : ChartMetrics -> God -> God -> (Point, DuoConnectorShape)
calculateAdjacent metrics a b =
  let
    endA = godCenter metrics a
    endB = godCenter metrics b
    iconPoint = Geometry.midpoint endA endB
    {-
    angleA = Debug.log "god a angle" (angleOfGod metrics a)
    angleB = Debug.log "god b angle" (angleOfGod metrics b)
    angle = (Geometry.angleBetween iconPoint (0,1)) |> Debug.log "adjacent"
    _ = Debug.log "a" ((angle + angleA - (tau/2)) * 360 / tau)
    _ = Debug.log "b" ((angle + angleB) * 360 / tau)
    -}
  in
    (iconPoint, Invisible)

calculateSkipOne : ChartMetrics -> God -> God -> (Point, DuoConnectorShape)
calculateSkipOne =
  calculateArc -0.2 (mainRingRadius * 1.45)

calculateSkipTwo : ChartMetrics -> God -> God -> (Point, DuoConnectorShape)
calculateSkipTwo =
  calculateArc 0.2 (mainRingRadius * 1.6)

calculateOpposite : ChartMetrics -> God -> God -> (Point, DuoConnectorShape)
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
        Geometry.interpolate 0.1 endB endA
      else
        Geometry.interpolate 0.1 endA endB
  in
    (iconPoint, (DuoLine endA endB))

calculateArc : Float -> Float -> ChartMetrics -> God -> God -> (Point, DuoConnectorShape)
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
    {-
    relA = Geometry.sub centerA termA
    angleOriginA = Geometry.rotate (tau/4) centerA
    angleA = Geometry.angleBetween angleOriginA relA |> (\x -> x * 360 / tau) |> Debug.log "angle a"
    relB = Geometry.sub centerB termB
    angleOriginB = Geometry.rotate (tau/4) centerB
    angleB = Geometry.angleBetween angleOriginB relB |> (\x -> x * 360 / tau) |> Debug.log "angle b"
    iconRadius = Geometry.length iconPoint
    _ = Debug.log "offset" ((iconRadius - mainRingRadius - metrics.adjacentDistance/2) / (metrics.adjacentDistance/2))
    -}
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
  metrics.gods
    |> Array.get (godIndex who)
    |> Maybe.map .center
    |> Maybe.withDefault (0,0)

angleOfGod : ChartMetrics -> God -> Float
angleOfGod metrics who =
  metrics.gods
    |> Array.get (godIndex who)
    |> Maybe.map .angle
    |> Maybe.withDefault 0

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
    Hermes -> 0
    Aphrodite -> 1
    Ares -> 2
    Demeter -> 3
    Dionysus -> 4
    Poseidon -> 5
    Athena -> 6
    Artemis -> 7
    Zeus -> 8

colorForGod : ChartMetrics -> God -> Color
colorForGod {gods} who =
  gods
    |> Array.filter (\{god} -> god == who)
    |> Array.get 0
    |> Maybe.map .color
    |> Maybe.withDefault Color.white

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
