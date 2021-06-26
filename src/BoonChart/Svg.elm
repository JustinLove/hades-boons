module BoonChart.Svg exposing (BoonChart, boonChart)

import BoonChart exposing (..)
import Geometry
import Traits exposing (TraitId, Traits, Trait, GodData, God(..), BoonType(..), BoonStatus(..))
import Layout exposing (Layout, GroupId, Boundary(..), Winding(..))
import MouseWheel

import Array exposing (Array)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render
import Collage.Text as Text
import Collage.Events as Events
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Set exposing (Set)
import Svg
import Svg.Attributes

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
  }

tau = pi*2

boonChart : List (Svg.Attribute msg) -> BoonChart msg -> Html msg
boonChart attributes model =
  let
    metrics = model.metrics
    basicBoons = metrics.gods |> Array.toList |> List.concatMap .boons
    basicConnectors = metrics.gods |> Array.toList |> List.map .connectors
    duoSize = duoBoonSize model.zoom
    basicSize = basicBoonSize model.zoom
    displayDiameter = model.zoom * model.diameter
  in
  --[ circle 0.45
      --|> outlined (solid 0.01 (uniform Color.white))
  [ metrics.duoBoons |> List.map (displayBoonTrait displayDiameter duoSize model.boonStatus) |> stack
  , basicBoons |> List.map (displayBoonTrait displayDiameter basicSize model.boonStatus) |> stack
  , List.map2 (\active cons -> List.map (displayBoonConnector model.boonStatus active) cons |> stack)
      model.activeBasicGroups
      basicConnectors
      |> stack
  , displayGods metrics |> stack
  , metrics.duoConnectors |> List.map (displayDuoConnector model.activeDuoGroups) |> stack
  , rectangle 1 1
      |> filled (uniform Color.black)
      --|> styled (uniform Color.black, dot 0.001 (uniform Color.white))
      --|> outlined (solid 0.005 (uniform Color.white))
      --|> outlined (solid 0.005 (transparent))
  ]
    |> stack
    |> align topLeft
    |> List.singleton
    |> group
    --|> debug
    |> shift (flip model.offset)
    |> scale model.diameter
    |> scale model.zoom
    |> when (model.drag == Released) (Events.onMouseDown model.onMouseDown)
    |> when (model.drag /= Released) (Events.onMouseUp model.onMouseUp)
    |> when (model.drag /= Released) (Events.onMouseLeave model.onMouseUp)
    |> when (model.drag /= Released) (Events.onMouseMove model.onMouseMove)
    |> Collage.Render.svgExplicit ((MouseWheel.onWheel model.onWheel) :: attributes)

when : Bool -> (Collage msg -> Collage msg) -> Collage msg -> Collage msg
when test f collage =
  if test then f collage else collage

flip : Point -> Point
flip (x, y) = (x, -y)

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
  [ {-Text.fromString (godMetrics.name)
      |> Text.color (godMetrics.color)
      |> Text.size 200
      |> rendered
      |> scale 0.001
  , -}circle 0.5
      |> styled
        ( uniform (Color.rgba 0.0 0.0 0.0 0.7)
        , solid 0.01 (uniform (Color.rgb 0.05 0.05 0.05))
        )
  ]
    |> stack
    |> Collage.Layout.name (godMetrics.name)

displayBoonTrait : Float -> Float -> Dict TraitId BoonStatus -> Boon -> Collage msg
displayBoonTrait displayDiameter size boonStatus boon =
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
    textLine = \text sz ->
      let
        font = size * displayDiameter * sz
        f = (font - 6.0) / 8.0 |> atMost 1.0
        c = darken f color
      in
      if font > 6 then
        Text.fromString text
          |> Text.color c
          |> Text.size 100
          |> rendered
          |> scale (sz * 0.01)
      else
        group []
  in
  [ textLine boon.name 0.2
      |> shiftY -0.5
  , textLine boon.id 0.1
      |> shiftY -0.65
  , if status == Excluded then
      image (0.7, 0.7) "GUI/LockIcon/LockIcon0001.png"
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
    |> scale size

displayBoonConnector : Dict TraitId BoonStatus -> Set GroupId -> Connector -> Collage msg
displayBoonConnector boonStatus activeGroups {shape, link, group, color} =
  let
    bright = uniform color
    dark = uniform (darken 0.4 color)
    (thickness, col) =
      if Set.member group activeGroups then
        case link of
          Just id ->
            if Dict.get id boonStatus == Just Active || Set.member id activeGroups then
              (0.004, bright)
            else
              (0.001, dark)
          Nothing ->
            (0.004, bright)
      else
        case link of
          Just id ->
            (0.001, dark)
          Nothing ->
            (0.002, dark)
    lineStyle = solid thickness col
  in
  case shape of
    Arc arcInfo ->
      arcFromAngles arcInfo
        |> traced lineStyle
    Area boundaries ->
      boundaries
        |> List.map (\bound ->
          case bound of
            BoundaryArc arc ->
              curvePointsFromAngles arc
            BoundaryLine a b ->
              curvePointsForLine a b
          )
        |> List.foldr joinCurvePoints []
        |> cubicCurve
        |> close
        |> filled col
    Circle center radius ->
      circle radius
        |> outlined lineStyle
        |> shift center
    EllipticArc arcInfo ->
      ellipticArcFromAngles arcInfo
        |> traced lineStyle
    Line a b ->
      segment a b
        |> traced lineStyle
    PolyLine points ->
      path points
        |> traced lineStyle
    Tag ->
      Collage.group []

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

curvePoints : Point -> Point -> Point -> List Point
curvePoints (xc,yc) (x1,y1) (x4,y4) =
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

curvePointsFromAngles : ArcType -> List Point
curvePointsFromAngles {center, radius, fromAngle, toAngle, winding} =
  let
    oddAngle = abs (toAngle - fromAngle)
    angle =
      if oddAngle > pi then
        tau - oddAngle
      else
        oddAngle
    (startAngle, workAngle, endAngle) =
      case winding of
        Counterclockwise ->
          (fromAngle, fromAngle + angle / 2, toAngle)
        Clockwise ->
          (tau - fromAngle, tau - (fromAngle + angle / 2), tau - toAngle)
    midAngle =
      if workAngle > tau then
        workAngle - tau
      else
        workAngle
    midPoint = center |> Geometry.add (Geometry.rotate midAngle (radius, 0))
    endA = center |> Geometry.add (Geometry.rotate startAngle (radius, 0))
    endB = center |> Geometry.add (Geometry.rotate endAngle (radius, 0))
  in
    if angle > tau/4 then
      joinCurvePoints
        (curvePoints center endA midPoint)
        (curvePoints center midPoint endB)
    else
      (curvePoints center endA endB)


{- Adapted from https://www.blog.akhil.cc/ellipse
However, that page appears to use a y inverse coordinate system, and the tanget formula is incorrect. It references
  http://www.spaceroots.org/documents/ellipse/elliptical-arc.pdf
Which has the correct tangent formula and appears to use same y polarity
-}
curvePointsFromEllipticAngles : EllipticArcType -> List Point
curvePointsFromEllipticAngles {center, majorAxis, minorRatio, fromAngle, toAngle} =
  let
    (cosT, sinT) = majorAxis |> Geometry.normalize
    semiMajor = Geometry.length majorAxis
    semiMinor = semiMajor * minorRatio
    ellipsePoint = \n ->
      let
        cosN = cos n
        sinN = sin n
        a = semiMajor
        b = semiMinor
      in
        ( a * cosN * cosT - b * sinN * sinT
        , a * cosN * sinT + b * sinN * cosT
        )
          |> Geometry.add center
    tangent = \n ->
      let
        cosN = cos n
        sinN = sin n
        a = semiMajor
        b = semiMinor
      in
        ( -a * sinN * cosT - b * cosN * sinT
        , -a * sinN * sinT + b * cosN * cosT
        )
    startAngle = fromAngle
    endAngle = toAngle
    oddAngle = abs (toAngle - fromAngle)
    angle =
      if oddAngle > pi then
        tau - oddAngle
      else
        oddAngle
    workAngle = fromAngle + oddAngle / 2
    midAngle = Geometry.modAngle workAngle
    midPoint = ellipsePoint midAngle
    endA = ellipsePoint startAngle
    --endB = ellipsePoint endAngle
    ellipsePoints = \from to ->
      let
        step = to - from
        at = tan (step/2)
        a = (sin step) * ((sqrt (4 + 3*at*at)-1)/3)
        p1 = ellipsePoint from
        e1 = tangent from
        p2 = ellipsePoint to
        e2 = tangent to
        q1 = Geometry.scale a e1 |> Geometry.add p1
        q2 = Geometry.scale -a e2 |> Geometry.add p2
      in
        [p1, q1, q2, p2]
  in
    if angle > tau/4 then
      joinCurvePoints
        (ellipsePoints startAngle midAngle)
        (ellipsePoints midAngle endAngle)
    else
      (ellipsePoints startAngle endAngle)

angleOf : Point -> Float
angleOf (x,y) =
  -(atan2 x y) + tau/4

curvePointsForLine : Point -> Point -> List Point
curvePointsForLine a b =
  [a, a, b, b]

joinCurvePoints : List Point -> List Point -> List Point
joinCurvePoints a b =
  List.append a (List.drop 1 b)

arcFromPoints : Point -> Point -> Point -> Path
arcFromPoints c p1 p2 =
  cubicCurve (curvePoints c p1 p2)

arcFromAngles : ArcType -> Path
arcFromAngles arcType =
  cubicCurve (curvePointsFromAngles arcType)

ellipticArcFromAngles : EllipticArcType -> Path
ellipticArcFromAngles arcType =
  cubicCurve (curvePointsFromEllipticAngles arcType)

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
