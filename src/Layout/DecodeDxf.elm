module Layout.DecodeDxf exposing
  ( layout
  , placement
  , point
  )

import Layout exposing (..)
import Dxf exposing (..)
import Dxf.Decode exposing (..)

layout : Decoder Layout
layout =
  entities Point placement

placement : Decoder Placement
placement =
  succeed Placement
    |> with id
    |> with point

id : Decoder String
id =
  andThen extractId (tag 1000 text)

extractId : String -> Decoder String
extractId =
  String.split(":")
    >> List.drop 1
    >> List.head
    >> Maybe.map succeed
    >> Maybe.withDefault (fail "point not tagged")


point : Decoder (Float, Float)
point =
  succeed Tuple.pair
    |> with (tag 10 x)
    |> with (tag 20 y)
