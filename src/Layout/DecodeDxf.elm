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
    |> with (entities TextEntity placement)
    |> with connections
    |> with layoutRadius

placement : Decoder Placement
placement =
  succeed Placement
    |> with idFromText
    |> with (pointBase 11)

connections : Decoder (List Connection)
connections =
  succeed (\a c l p h -> List.concat [a, c, l, p, h] |> removeGuidelines)
    |> with (entities ArcEntity (connection arc))
    |> with (entities CircleEntity (connection circle))
    |> with (entities LineEntity (connection line))
    |> with (entities LWPolyLineEntity (connection polyline))
    |> with (entities HatchEntity (connection area))

connection : Decoder ConnectionType ->  Decoder Connection
connection decoder =
  succeed Connection
    |> with inLayer
    |> with (maybe idFromTag)
    |> with decoder

arc : Decoder ConnectionType
arc =
  succeed ArcType
    |> with (pointBase 10)
    |> with (tag 40 floatValue)
    |> with (tag 50 radians)
    |> with (tag 51 radians)
    |> map Arc

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

polyline : Decoder ConnectionType
polyline =
  succeed PolyLine
    |> with (every 10 (pointBase 10))

area : Decoder ConnectionType
area =
  succeed Area
    |> with (every 72 boundaryData)

boundaryData : Decoder ConnectionType
boundaryData =
  tag 72 intValue
    |> andThen (\edgeType ->
      case edgeType of
        1 -> line
        2 -> arc
        3 -> fail "elliptic arc"
        4 -> fail "spline"
        _ -> fail "invalid edge type"
      )

inLayer : Decoder String
inLayer =
  tag 8 layer
    |> map (String.replace "C ... " "")

idFromText : Decoder String
idFromText =
  tag 1 primaryText

idFromTag : Decoder String
idFromTag =
  andThen extractId (tag 1000 text)

extractId : String -> Decoder String
extractId s =
  case String.split(":") s of
    "link" :: link :: [] -> succeed link
    _ -> fail "link not tagged"

pointBase : Int -> Decoder (Float, Float)
pointBase base =
  succeed Tuple.pair
    |> with (tag base x)
    |> with (tag (base+10) y)

radians : Value -> Result Error Float
radians v =
  angle v
    |> Result.map (\deg -> deg / 360 * pi * 2)

removeGuidelines : List Connection -> List Connection
removeGuidelines list =
  List.filter (\{group} -> group /= "Guidelines" && group /= "LayoutRadius") list

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
