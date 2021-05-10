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
    |> with (entities Line connection |> map removeGuidelines)

placement : Decoder Placement
placement =
  succeed Placement
    |> with idFromText
    |> with (pointBase 11)

connection : Decoder Connection
connection =
  succeed Connection
    |> with inLayer
    |> with (maybe idFromTag)
    |> with (pointBase 10)
    |> with (pointBase 11)

inLayer : Decoder String
inLayer =
  tag 8 layer

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

removeGuidelines : List Connection -> List Connection
removeGuidelines list =
  List.filter (\{group} -> group /= "Guidelines") list
