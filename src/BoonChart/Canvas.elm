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
        |> (::) "GUI/Screens/BoonIconFrames/common.png"
        |> (::) "GUI/Screens/BoonIconFrames/primary.png"
        |> List.append (metrics.gods
          |> Array.toList
          |> List.map .god
          |> List.map Traits.godIcon
          )
        |> List.append (Traits.slots
          |> List.map Traits.iconForSlot
          )
        |> List.map (\path -> Canvas.loadFromImageUrl path (model.onTexture path))
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
    --, displayPoint 1 model.offset
    --, shapes [ fill Color.white ] [ circle model.offset 4 ]
    , group
      [ transform
        [ translate (flip model.offset)
        , scale displayDiameter
        , translate (0.5, -0.5)
        ]
      ]
      (List.concat
        [ metrics.duoConnectors |> List.concatMap (displayDuoConnector model.activeDuoGroups)
        , displayGods model.textures model.zoom metrics
        , List.map2 (\active cons -> List.map (displayBoonConnector model.boonStatus active) cons)
            model.activeBasicGroups
            basicConnectors
            |> List.concat
        , basicBoons |> List.map (displayBoonTrait displayDiameter basicSize model.textures model.boonStatus)
        , metrics.duoBoons |> List.map (displayBoonTrait displayDiameter duoSize model.textures model.boonStatus)
        --, [ shapes [ stroke Color.white , lineWidth 0.01 ] [ circle (0, 0) 0.5 ] ]
        --, metrics.gods
            --|> Array.toList
            --|> List.map (\godMetrics ->
              --displayPoint displayDiameter (flip godMetrics.center)
            --)
        ]
      )
    ]

displayPoint : Float -> Point -> Renderable
displayPoint size (x,y) =
  group
    []
    [ shapes [ fill Color.white ] [ circle (x,y) (4/size) ]
    , text
      [ fill Color.white
      , font { size = 24, family = "sans-serif" }
      , baseLine Top
      , transform [ scale (1/size) ]
      ]
      (x*size,y*size) ((printFloat x) ++ "," ++ (printFloat y))
    ]

printFloat : Float -> String
printFloat x =
  (x * 100)
    |> truncate
    |> (\y -> (toFloat y) / 100)
    |> String.fromFloat

when : Bool -> Html.Attribute msg -> Html.Attribute msg
when test att =
  if test then att else Html.Attributes.class ""

flip : Point -> Point
flip (x, y) = (x, -y)

displayGods : Dict String Canvas.Texture -> Float -> ChartMetrics -> List Renderable
displayGods textures zoom metrics =
  metrics.gods
    |> Array.toList
    |> List.map (\godMetrics ->
      displayGod textures zoom metrics.adjacentDistance godMetrics
        |> group
          [ transform
            [ translate godMetrics.center
            , rotate godMetrics.angle
            , scale metrics.adjacentDistance
            ]
          ]
    )

displayGod : Dict String Canvas.Texture -> Float -> Float -> GodMetrics -> List Renderable
displayGod textures zoom adjacentDistance godMetrics =
  [ shapes
    [ fill (Color.rgba 0.0 0.0 0.0 0.7)
    , stroke (Color.rgb 0.05 0.05 0.05)
    , lineWidth 0.01
    ]
    [ circle (0,0) 0.5 ]
  , Dict.get (Traits.godIcon godMetrics.god) textures
    |> Maybe.map (\tex ->
      let {width, height} = Canvas.dimensions tex in
      texture
        [ transform
          [ scale (godMetrics.scaleFactor/adjacentDistance)
          , translate (-width/2, height/2)
          ]
        , Canvas.alpha (1/(zoom^1.1))
        ]
        (0,0)
        tex
      )
    |> Maybe.withDefault (group [] [])
  {-, text
    [ fill godMetrics.color
    , font { size = 200, family = "sans-serif" }
    , align Center
    , transform [ scale 0.001 ]
    ]
    (0, 0) godMetrics.name
    -}
  ]

displayBoonTrait : Float -> Float -> Dict String Canvas.Texture -> Dict TraitId BoonStatus -> Boon -> Renderable
displayBoonTrait displayDiameter boonSize textures boonStatus boon =
  let
    size =
      case boon.iconType of
        Direct -> boonSize
        Slot -> boonSize * 0.8
        Reference -> boonSize * 0.5
        Keepsake -> boonSize
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
        Unavailable -> 0.1
        Excluded -> 0.1
    overlayColor =
      case status of
        Active -> Color.rgba 1 1 1 0.7
        Available -> Color.rgba 0 0 0 0
        Unavailable -> Color.rgba 0 0 0 0.5
        Excluded -> Color.rgba 0 0 0 0.8
    smallColor =
      case status of
        Active -> Color.white
        Available -> boon.color
        Unavailable -> boon.color |> darken 0.5
        Excluded -> Color.charcoal
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
    displaySize = size * displayDiameter
      --|> Debug.log "displaysize"
  in
  [ if 15.0 < displaySize then
      if boon.iconType == Keepsake then
        group []
          [ image [Canvas.alpha brightness] textures 0.9 boon.icon
          ]
      else
        group []
          [ image [] textures 0.9 boon.icon
          , if overlayColor /= (Color.rgba 0 0 0 0) then
              shapes
                [ fill overlayColor
                , transform
                  [ translate (-0.47, 0.0)
                  , rotate (tau/8)
                  ]
                ]
                [ rect (0,0) side side
                ]
            else
              group [] []
          , case status of
              Active ->
                image [] textures 1.2 "GUI/Screens/BoonIconFrames/common.png"
              Available ->
                image [] textures 1.2 "GUI/Screens/BoonIconFrames/primary.png"
              Excluded ->
                image [] textures 0.7 "GUI/LockIcon/LockIcon0001.png"
              Unavailable ->
                group [] []
          ]
    else
      group [] []
  , if displaySize < 25.0 then
      shapes
        [ fill smallColor
        , transform
          [ translate (-0.47, 0.0)
          , rotate (tau/8)
          ]
        , Canvas.alpha (1.0 - (((displaySize - 15.0) / 10.0) |> clamp 0 1))
        ]
        [ rect (0,0) side side
        ]
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

displayBoonConnector : Dict TraitId BoonStatus -> Set GroupId -> Connector -> Renderable
displayBoonConnector boonStatus activeGroups {shape, link, group, color} =
  let
    bright = color
    dark = darken 0.4 color
    (thickness, col) =
      if Set.member group activeGroups then
        case link of
          [] ->
            (0.004, bright)
          _ ->
            if List.all (\id -> Dict.get id boonStatus == Just Active || Set.member id activeGroups) link then
              (0.004, bright)
            else
              (0.001, dark)
      else
        case link of
          [] ->
            (0.002, dark)
          _ ->
            (0.001, dark)
    lineStyle =
      [ lineWidth thickness
      , lineCap RoundCap
      , stroke col
      ]
  in
  case shape of
    Arc arcInfo ->
      shapes
        lineStyle
        [ arc (flip arcInfo.center) arcInfo.radius
          { startAngle = -arcInfo.fromAngle
          , endAngle = -arcInfo.toAngle
          , clockwise = arcInfo.winding == Clockwise
          }
        ]
    Area boundaries ->
      let
        segments = boundaries
          |> List.map (\bound ->
            case bound of
              BoundaryArc arc ->
                pathFromAngles arc
              BoundaryLine a b ->
                ((flip a), [lineTo (flip b)])
            )
      in
        case segments of
          first :: _ ->
            shapes
              [ fill col, Canvas.alpha 0.5 ]
              [ path
                (Tuple.first first)
                (List.concatMap Tuple.second segments)
              ]
          _ ->
            Canvas.group [] []
    Circle center radius ->
      shapes
        lineStyle
        [ circle (flip center) radius ]
    EllipticArc arcInfo ->
      shapes
        lineStyle
        [pathFromEllipticAngles arcInfo]
    Line a b ->
      shapes
        lineStyle
        [ path (flip a) [lineTo (flip b)] ]
    PolyLine points ->
      case points of
        first :: rest ->
          shapes
            lineStyle
            [ path (flip first) (List.map (flip >> lineTo) rest) ]
        _ ->
          Canvas.group [] []
    Tag ->
      Canvas.group [] []

image : List Setting -> Dict String Canvas.Texture -> Float -> String -> Renderable
image settings textures size key =
  Dict.get key textures
    |> Maybe.map (\tex ->
      let {width, height} = Canvas.dimensions tex in
      texture
        (List.append settings
          [ transform
            [ scale (size/(max width height))
            , translate (-width/2, height/2)
            ]
          ]
        )
        (0,0)
        tex
      )
    |> Maybe.withDefault (group [] [])

displayDuoConnector : Set GroupId -> DuoConnector -> List Renderable
displayDuoConnector activeGroups {shape, groupA, colorA, groupB, colorB} =
  let
    lineStyleA = duoDash False activeGroups groupA colorA
    lineStyleB = duoDash True activeGroups groupB colorB
  in
  case shape of
    Invisible ->
      []
    DuoArc arc ->
      let
        arcA = arcFromPoints arc.center arc.endA arc.midPoint
        arcB = arcFromPoints arc.center arc.midPoint arc.endB
      in
      [ shapes
        lineStyleA
        [ arcA, arcB ]
      , shapes
        lineStyleB
        [ arcA, arcB ]
      ]
    DuoLine a b->
      [ shapes
        lineStyleA
        [ path (flip a) [lineTo (flip b)] ]
      , shapes
        lineStyleB
        [ path (flip a) [lineTo (flip b)] ]
      ]

duoDash : Bool -> Set GroupId -> GroupId -> Color -> List Setting
duoDash which activeGroups group godColor =
  let
    (thickness, color) =
      if Set.member group activeGroups then
        (0.004, godColor)
      else
        (0.002, Color.charcoal)
  in
    [ lineDash [ 0.01, 0.01 ]
    , lineDashOffset (if which then 0 else 0.01)
    , lineWidth thickness
    , stroke color
    ]

curvePath : Point -> Point -> Point -> PathSegment
curvePath (xc,yc) (x1,y1) (x4,y4) =
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
  bezierCurveTo (x2,-y2) (x3,-y3) (x4,-y4)

pathFromAngles : ArcType -> (Point, List PathSegment)
pathFromAngles {center, radius, fromAngle, toAngle, winding} =
  let
    angle = abs (toAngle - fromAngle)
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
    if angle > tau/2 then
      let
        (_, paths1) = 
          pathFromAngles
            { center = center
            , radius = radius
            , fromAngle = startAngle
            , toAngle = midAngle
            , winding = winding
            }
        (_, paths2) = 
          pathFromAngles
            { center = center
            , radius = radius
            , fromAngle = midAngle
            , toAngle = endAngle
            , winding = winding
            }
      in
        ((flip endA), List.append paths1 paths2)
    else if angle > tau/4 then
      (endA,
        [ (curvePath center endA midPoint)
        , (curvePath center midPoint endB)
        ]
      )
    else
      (endA, [curvePath center endA endB])

{- Adapted from https://www.blog.akhil.cc/ellipse
However, that page appears to use a y inverse coordinate system, and the tanget formula is incorrect. It references
  http://www.spaceroots.org/documents/ellipse/elliptical-arc.pdf
Which has the correct tangent formula and appears to use same y polarity
-}
pathFromEllipticAngles : EllipticArcType -> Shape
pathFromEllipticAngles {center, majorAxis, minorRatio, fromAngle, toAngle} =
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
    ellipsePath = \from to ->
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
        bezierCurveTo (flip q1) (flip q2) (flip p2)
  in
    if angle > tau/4 then
      path (flip endA)
        [ (ellipsePath startAngle midAngle)
        , (ellipsePath midAngle endAngle)
        ]
    else
      path (flip endA) [ellipsePath startAngle endAngle]

arcFromPoints : Point -> Point -> Point -> Shape
arcFromPoints c p1 p2 =
  path (flip p1) [curvePath c p1 p2]

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
