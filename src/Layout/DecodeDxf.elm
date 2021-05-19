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

placement : Decoder Placement
placement =
  succeed Placement
    |> with idFromText
    |> with (pointBase 11)

connections : Decoder (List Connection)
connections =
  succeed (\a b -> List.append a b |> removeGuidelines)
    |> with (entities LineEntity (connection line))
    |> with (entities ArcEntity (connection arc))

connection : Decoder ConnectionType ->  Decoder Connection
connection decoder =
  succeed Connection
    |> with inLayer
    |> with (maybe idFromTag)
    |> with decoder

line : Decoder ConnectionType
line =
  succeed Line
    |> with (pointBase 10)
    |> with (pointBase 11)

arc : Decoder ConnectionType
arc =
  succeed ArcType
    |> with (pointBase 10)
    |> with (tag 40 floatValue)
    |> with (tag 50 radians)
    |> with (tag 51 radians)
    |> map Arc

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
extractId =
  String.split(":")
    >> List.drop 1
    >> List.head
    >> Maybe.map succeed
    >> Maybe.withDefault (fail "point not tagged")

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
  List.filter (\{group} -> group /= "Guidelines") list
