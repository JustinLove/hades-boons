module Layout.DecodeDxf exposing
  ( layout
  , placement
  , pointBase
  )

import Layout exposing (..)
import Dxf exposing (..)
import Dxf.Decode exposing (..)

layout : Decoder Layout
layout =
  succeed Layout
    |> with placements
    |> with connections
    |> with layoutRadius

placements : Decoder (List Placement)
placements =
  entities TextEntity maybePlacement
    |> map (List.filterMap identity)

maybePlacement : Decoder (Maybe Placement)
maybePlacement =
  inLayer
    |> andThen (\layer ->
      if layer == "Duos" then
        succeed Nothing
      else
        map Just placement
      )

placement : Decoder Placement
placement =
  succeed Placement
    |> with idFromText
    |> with (pointBase 11)

connections : Decoder (List Connection)
connections =
  succeed (\a c l p h e d -> List.concat [a, c, l, p, h, e, d] |> removeGuidelines)
    |> with (entities ArcEntity (connection arc))
    |> with (entities CircleEntity (connection circle))
    |> with (entities LineEntity (connection line))
    |> with (entities LWPolyLineEntity (connection polyline))
    |> with (entities HatchEntity (connection area))
    |> with (entities EllipseEntity (connection ellipticArc))
    |> with (entities PointEntity (connection point))

connection : Decoder ConnectionType ->  Decoder Connection
connection decoder =
  succeed Connection
    |> with inLayer
    |> with idFromTag
    |> with decoder

arc : Decoder ConnectionType
arc =
  succeed ArcType
    |> with (pointBase 10)
    |> with (tag 40 floatValue)
    |> with (tag 50 radians)
    |> with (tag 51 radians)
    |> with (succeed Counterclockwise)
    |> map Arc

boundaryArc : Decoder Boundary
boundaryArc =
  succeed ArcType
    |> with (pointBase 10)
    |> with (tag 40 floatValue)
    |> with (tag 50 radians)
    |> with (tag 51 radians)
    |> with (tag 73 winding)
    |> map BoundaryArc

ellipticArc : Decoder ConnectionType
ellipticArc =
  succeed EllipticArcType
    |> with (pointBase 10)
    |> with (pointBase 11)
    |> with (tag 40 floatValue)
    |> with (tag 41 floatValue)
    |> with (tag 42 floatValue)
    |> map EllipticArc

circle : Decoder ConnectionType
circle =
  succeed Circle
    |> with (pointBase 10)
    |> with (tag 40 floatValue)

line : Decoder ConnectionType
line =
  succeed Line
    |> with (pointBase 10)
    |> with (pointBase 11)

boundaryLine : Decoder Boundary
boundaryLine =
  succeed BoundaryLine
    |> with (pointBase 10)
    |> with (pointBase 11)

polyline : Decoder ConnectionType
polyline =
  succeed PolyLine
    |> with (every 10 (pointBase 10))

area : Decoder ConnectionType
area =
  succeed Area
    |> with (every 72 boundaryData)

boundaryData : Decoder Boundary
boundaryData =
  tag 72 intValue
    |> andThen (\edgeType ->
      case edgeType of
        1 -> boundaryLine
        2 -> boundaryArc
        3 -> fail "elliptic arc"
        4 -> fail "spline"
        _ -> fail "invalid edge type"
      )

point : Decoder ConnectionType
point =
  pointBase 10
    |> andThen(\p -> succeed (Line p p))

inLayer : Decoder String
inLayer =
  tag 8 layer
    |> map (String.replace "C ... " "")

idFromText : Decoder String
idFromText =
  tag 1 primaryText

idFromTag : Decoder (List String)
idFromTag =
  oneOf
    [ (tag 1000 text) |> andThen extractId
    , succeed []
    ]

extractId : String -> Decoder (List String)
extractId s =
  case String.split(":") s of
    "link" :: link :: [] -> succeed (String.split(",") link)
    _ -> succeed []

pointBase : Int -> Decoder (Float, Float)
pointBase base =
  succeed Tuple.pair
    |> with (tag base x)
    |> with (tag (base+10) y)

radians : Value -> Result Error Float
radians v =
  angle v
    |> Result.map (\deg -> deg / 360 * pi * 2)

winding : Value -> Result Error Winding
winding v =
  intValue v
    |> Result.map (\flag ->
      case flag of
        0 -> Clockwise
        _ -> Counterclockwise
      )

removeGuidelines : List Connection -> List Connection
removeGuidelines list =
  List.filter (\{group} -> group /= "Guidelines" && group /= "LayoutRadius") list

removeDuos : List (Maybe Placement) -> List Placement
removeDuos list =
  --List.filter (\{group} -> group /= "Duos") list
  List.filterMap identity list

layoutRadius : Decoder Float
layoutRadius =
  entities CircleEntity outerCircle
    |> andThen (\candidates ->
      case List.filterMap identity candidates of
        first :: _ -> succeed first
        _ -> fail "no radius found"
    )

outerCircle : Decoder (Maybe Float)
outerCircle list =
  (inLayer
    |> andThen (\group ->
      if group == "LayoutRadius" then
        (tag 40 floatValue) |> map Just
      else
        succeed Nothing
    )
  ) list
