module BoonChart exposing
  ( ArcType
  , Boon
  , IconType(..)
  , ChartMetrics
  , Connector
  , ConnectorShape(..)
  , DuoConnector
  , DuoConnectorShape(..)
  , DragMode(..)
  , EllipticArcType
  , GodMetrics
  , basicBoonSize
  , calculateMetrics
  , duoBoonSize
  , focusAngleOf
  , focusPositionOf
  , hitChart
  )

import Geometry
import Layout exposing (GroupId, Boundary(..), Winding(..))
import Traits exposing (TraitId, Traits, Trait, God(..), GodData, BoonType(..), Frame(..), SlotId)

import Array exposing (Array)
import Color exposing (Color)
import Set

type alias Point = (Float, Float)

type DragMode
  = Released
  | Dragging Point Point

type alias ChartMetrics =
  { gods : Array GodMetrics
  , duoBoons : List Boon
  , duoReferenceBoons : List Boon
  , duoConnectors : List DuoConnector
  , angle : Float
  , adjacentDistance : Float
  }

type alias GodMetrics =
  { center : Point
  , angle : Float
  , focusAngle : Float
  , scaleFactor : Float
  , god : God
  , name : String
  , color : Color
  , boons : List Boon
  , connectors : List Connector
  }

type alias Boon =
  { name : String
  , icon : String
  , id : String
  , location : Point
  , iconType : IconType
  , frame : Frame
  , color : Color
  }

type IconType
  = Direct
  | Slot
  | Reference
  | DuoReference

type alias Connector =
  { shape : ConnectorShape
  , link : List TraitId
  , group : String
  , color : Color
  }

type ConnectorShape
  = Arc ArcType
  | Area (List Boundary)
  | Circle Point Float
  | EllipticArc EllipticArcType
  | Line Point Point
  | PolyLine (List Point)
  | Tag

type alias ArcType =
  { center : Point
  , radius : Float
  , fromAngle : Float
  , toAngle : Float
  , winding : Winding
  }

type alias EllipticArcType =
  { center : Point
  , majorAxis : Point
  , minorRatio : Float
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

mainRingRadius = 0.28

tau = pi*2

focusAngleOf : ChartMetrics -> God -> Float
focusAngleOf {gods} target =
  gods
    |> Array.filter (\{god} -> god == target)
    |> Array.get 0
    |> Maybe.map .focusAngle
    |> Maybe.withDefault 0

focusPositionOf : ChartMetrics -> God -> Point
focusPositionOf {gods} target =
  gods
    |> Array.filter (\{god} -> god == target)
    |> Array.get 0
    |> Maybe.map .center
    |> Maybe.withDefault (0,0)

calculateMetrics : Traits -> Float -> ChartMetrics
calculateMetrics traits rotation =
  let
    metrics = initialMetrics traits rotation
    {duoBoons, duoReferenceBoons, duoConnectors} = layoutDuoBoons metrics (Traits.duoBoons traits)
  in
    { metrics
    | duoBoons = duoBoons
    , duoReferenceBoons = duoReferenceBoons
    , duoConnectors = duoConnectors
    }

initialMetrics : Traits -> Float -> ChartMetrics
initialMetrics traits rotation =
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
          layout = Traits.dataLayout data
          scaleFactor = (adjacentDistance/2)/layout.radius
          focus =
            if i == 0 then angle/2
            else (((toFloat (i-1)) * -angle))
          a =
            if i == 0 then 0
            else (focus - rotation)
          center =
            if i == 0 then (0,0)
            else (0, mainRingRadius) |> Geometry.rotate a
        in
          { center = center
          , angle = a
          , focusAngle = focus
          , scaleFactor = scaleFactor
          , god = Traits.dataGod data
          , name = Traits.dataName data
          , color = Traits.dataLootColor data
          , boons = layoutBasicBoonsOf traits (adjacentDistance/2) center a data
          , connectors = layoutBasicConnectorsOf (adjacentDistance/2) center a data
          }
      )
      |> Array.fromList
    , duoBoons = []
    , duoReferenceBoons = []
    , duoConnectors = []
    , angle = angle
    , adjacentDistance = adjacentDistance
    }

layoutDuoBoons : ChartMetrics -> List Trait -> {duoBoons : List Boon, duoReferenceBoons : List Boon, duoConnectors: List DuoConnector}
layoutDuoBoons metrics traits =
  let
    duos = traits |> List.map (layoutDuoBoon metrics)
  in
    { duoBoons = duos |> List.map .duoBoon
    , duoReferenceBoons = duos |> List.concatMap .duoReferenceBoons
    , duoConnectors = duos |> List.map .duoConnector
    }

layoutDuoBoon : ChartMetrics -> Trait -> {duoBoon: Boon, duoReferenceBoons : List Boon, duoConnector : DuoConnector}
layoutDuoBoon metrics trait =
  let
    (iconPoint, shape) = calculateDuo metrics trait
    (godA, godB) = case trait.boonType of
      BasicBoon g -> (g, g)
      DuoBoon a b -> (a, b)
      Keepsake -> (Hades, Hades)
    referencePoints = case shape of
      DuoArc {endA, endB} -> [endA, endB]
      DuoLine endA endB -> [endA, endB]
      Invisible -> []
  in
    { duoBoon =
      { name = trait.name
      , icon = trait.icon
      , id = trait.trait
      , location = iconPoint
      , iconType = Direct
      , frame = trait.frame
      , color = duoBoonColor
      }
    , duoReferenceBoons = 
     referencePoints
      |> List.map (\point ->
          { name = trait.name
          , icon = trait.icon
          , id = trait.trait
          , location = point
          , iconType = DuoReference
          , frame = trait.frame
          , color = duoBoonColor
          }
        )
    , duoConnector =
      { shape = shape
      , groupA = (Traits.godName godA) ++ trait.trait
      , colorA = colorForGod metrics godA
      , groupB = (Traits.godName godB) ++ trait.trait
      , colorB = colorForGod metrics godB
      }
    }

calculateDuo : ChartMetrics -> Trait -> (Point, DuoConnectorShape)
calculateDuo metrics trait =
  case trait.boonType of
    DuoBoon a b ->
      case godAdjacency (Array.length metrics.gods) a b of
        Adjacent -> calculateAdjacent metrics a b
        SkipOne -> calculateSkipOne metrics a b
        SkipTwo -> calculateSkipTwo metrics a b
        Opposite -> calculateOpposite metrics a b
    _ -> ((0,0), Invisible)

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

angleOfGod : ChartMetrics -> God -> Float
angleOfGod metrics who =
  metrics.gods
    |> Array.get (godIndex who)
    |> Maybe.map .angle
    |> Maybe.withDefault 0

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

layoutBasicConnectorsOf : Float -> Point -> Float -> GodData -> List Connector
layoutBasicConnectorsOf godRadius origin godAngle data =
  let
    layout = Traits.dataLayout data
    scaleFactor = godRadius/layout.radius
    toScale = Geometry.scale scaleFactor >> Geometry.rotate godAngle >> Geometry.add origin
    mapShape : Layout.ConnectionType -> ConnectorShape
    mapShape shape =
      case shape of
        Layout.Arc {center, radius, fromAngle, toAngle, winding} ->
          Arc
            { center = center |> toScale
            , radius = radius * scaleFactor
            , fromAngle = fromAngle + godAngle
            , toAngle = toAngle + godAngle
            , winding = winding
            }
        Layout.Circle center radius ->
          Circle
            (center |> toScale)
            (radius * scaleFactor)
        Layout.Dot p ->
          Tag
        Layout.EllipticArc {center, majorAxis, minorRatio, fromAngle, toAngle} ->
          EllipticArc
            { center = center |> toScale
            , majorAxis = majorAxis
              |> Geometry.scale scaleFactor
              |> Geometry.rotate godAngle
            , minorRatio = minorRatio
            , fromAngle = fromAngle
            , toAngle = toAngle
            }
        Layout.Line a b ->
          Line (a |> toScale) (b |> toScale)
        Layout.PolyLine points ->
          PolyLine (points |> List.map toScale)
        Layout.Area boundaries ->
          Area (boundaries |> List.map mapBoundary)
    mapBoundary shape =
      case shape of
        Layout.BoundaryArc {center, radius, fromAngle, toAngle, winding} ->
          let
            a = case winding of
              Counterclockwise -> godAngle
              Clockwise -> -godAngle
          in
          BoundaryArc
            { center = center |> toScale
            , radius = radius * scaleFactor
            , fromAngle = fromAngle + a
            , toAngle = toAngle + a
            , winding = winding
            }
        Layout.BoundaryLine a b ->
          BoundaryLine (a |> toScale) (b |> toScale)
  in
    data
      |> Traits.dataLayout
      |> .connections
      |> List.map (\{group, link, misc, shape} ->
        { shape = mapShape shape
        , link = link
        , group = group
        , color = if misc then Color.white else Traits.dataLootColor data
        }
      )
      |> List.filter (\{shape} -> shape /= Tag)

layoutBasicBoonsOf : Traits -> Float -> Point -> Float -> GodData -> List Boon
layoutBasicBoonsOf traits godRadius center godAngle data =
  let
    allTraits = Traits.allGods traits
      |> List.concatMap Traits.basicBoons
    layout = Traits.dataLayout data
    scaleFactor = godRadius/layout.radius
    boons = Traits.basicBoons data
    missing = Set.diff
      (List.map .trait boons |> Set.fromList)
      (List.map .id layout.placements |> Set.fromList)
    {-_ =
      if Set.isEmpty missing then
        missing
      else
        Debug.log "missing trait locations" missing
        -}
    angle = tau / (toFloat (List.length boons))
  in
    List.append
      (layout.placements
        |> List.map (\{id, point} ->
          let
            p = point
              |> Geometry.scale scaleFactor
              |> Geometry.rotate godAngle
              |> Geometry.add center
          in
          Traits.dataBoon data id
            |> Maybe.map (\trait ->
                { name = trait.name
                , icon = trait.icon
                , id = id
                , location = p
                , iconType = Direct
                , frame = trait.frame
                , color = Traits.dataLootColor data
                }
              )
            |> (\mboon ->
              case mboon of
                Just boon -> Just boon
                Nothing ->
                  if String.startsWith "Any" id then
                    let slot = String.dropLeft 3 id in
                    Just 
                      { name = Traits.nameForSlot slot
                      , icon = Traits.iconForSlot slot
                      , id = id
                      , location = p
                      , iconType = Slot
                      , frame = CommonFrame
                      , color = Color.charcoal
                      }
                  else
                    Nothing
              )
            |> (\mboon ->
              case mboon of
                Just boon -> Just boon
                Nothing ->
                  Traits.findBoon traits id
                    |> Maybe.map (\trait ->
                        { name = trait.name
                        , icon = trait.icon
                        , id = id
                        , location = p
                        , iconType = Reference
                        , frame = trait.frame
                        , color =
                          case trait.boonType of
                            BasicBoon g -> Traits.godColor g
                            DuoBoon _ _ -> duoBoonColor
                            Keepsake -> keepsakeColor
                        }
                      )
              )
            |> Maybe.withDefault
              { name = id
              , icon = "GUI/LockIcon/LockIcon0001.png"
              , id = id
              , location = p
              , iconType = Slot
              , frame = CommonFrame
              , color = Color.rgb255 23 25 21
              }
        )
      )
      (if Set.isEmpty missing then
        []
      else
        boons
          |> List.filter (\trait -> Set.member trait.trait missing)
          |> List.indexedMap (\i trait ->
              { name = trait.name
              , icon = trait.icon
              , id = trait.trait
              , location = (0, 0.05)
                |> Geometry.rotate (((toFloat i) * -angle) + -angle/2)
                |> Geometry.rotate godAngle
                |> Geometry.add center
              , iconType = Direct
              , frame = trait.frame
              , color = Traits.dataLootColor data
              }
          )
      )

colorForGod : ChartMetrics -> God -> Color
colorForGod {gods} who =
  gods
    |> Array.filter (\{god} -> god == who)
    |> Array.get 0
    |> Maybe.map .color
    |> Maybe.withDefault Color.white

godCenter : ChartMetrics -> God -> Point
godCenter metrics who =
  metrics.gods
    |> Array.get (godIndex who)
    |> Maybe.map .center
    |> Maybe.withDefault (0,0)

godIndex : God -> Int
godIndex who =
  case who of
    Charon -> 0
    Nyx -> 0
    Hades -> 0
    Hermes -> 0
    Aphrodite -> 1
    Ares -> 2
    Demeter -> 3
    Dionysus -> 4
    Poseidon -> 5
    Athena -> 6
    Artemis -> 7
    Zeus -> 8

hitChart : ChartMetrics -> Float -> Point -> Maybe TraitId
hitChart metrics zoom at =
  let
    point = at 
      |> Geometry.add (-0.5,-0.5)
      |> flip
    godHit = metrics.gods
      |> Array.toList
      |> List.filterMap (hitGod (metrics.adjacentDistance / 2) ((basicBoonSize zoom) / 2) point)
      |> List.head
    duoHit =
      case godHit of
        Just _ -> godHit
        Nothing -> hitBoons ((duoBoonSize zoom) / 2) point metrics.duoBoons
    duoRefHit =
      case duoHit of
        Just _ -> duoHit
        Nothing -> hitBoons ((basicBoonSize zoom) / 2) point metrics.duoReferenceBoons
  in
    duoRefHit

hitBoons : Float -> Point -> List Boon -> Maybe TraitId
hitBoons radius at boons =
  boons
    |> List.filter (hitBoon radius at)
    |> List.head
    |> Maybe.map .id

hitBoon : Float -> Point -> Boon -> Bool
hitBoon radius at {location, iconType} =
  case iconType of
    Direct ->
      Geometry.length (Geometry.sub at location) < radius
    Slot ->
      False
    Reference ->
      Geometry.length (Geometry.sub at location) < radius * 0.5
    DuoReference ->
      Geometry.length (Geometry.sub at location) < radius * 0.5

hitGod : Float -> Float -> Point -> GodMetrics -> Maybe TraitId
hitGod godRadius boonRadius at godMetrics =
  if Geometry.length (Geometry.sub at godMetrics.center) < godRadius then
    godMetrics.boons
      |> hitBoons boonRadius at
  else
    Nothing

duoBoonSize : Float -> Float
duoBoonSize zoom = (0.3 / zoom |> clamp 0.02 0.08)

basicBoonSize : Float -> Float
basicBoonSize zoom = (0.3 / zoom |> clamp 0.01 0.02)

duoBoonColor : Color
duoBoonColor = Color.rgb255 184 239 21

keepsakeColor : Color
keepsakeColor = Color.rgb255 236 210 104

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

flip : Point -> Point
flip (x, y) = (x, -y)
