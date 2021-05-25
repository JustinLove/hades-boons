module Layout.EncodeElm exposing (..)

import Layout exposing (..)

import Elm.CodeGen exposing (..)

type alias Point = (Float, Float)

layout : Layout -> Expression
layout l =
  record
    [ ("placements", list (List.map placement l.placements))
    , ("connections", list (List.map connection l.connections))
    , ("radius", l.radius |> float)
    ]

placement : Placement -> Expression
placement p =
  record
    [ ("id", p.id |> string)
    , ("point", p.point |> point)
    ]

point : Point -> Expression
point (x,y) =
  tuple [x |> float, y |> float]

connection : Connection -> Expression
connection c =
  record
    [ ("group", c.group |> string)
    , ("link", c.link |> maybe string)
    , ("shape", c.shape |> connectionType)
    ]

maybe : (a -> Expression) -> Maybe a -> Expression
maybe encoder m =
  case m of
    Just x -> construct "Just" [encoder x]
    Nothing -> val "Nothing"

connectionType : ConnectionType -> Expression
connectionType ct =
  case ct of
    Arc at -> construct "Arc" [arcType at]
    Circle c r -> construct "Circle" [point c, float r]
    Line a b -> construct "Line" [point a, point b]

arcType : ArcType -> Expression
arcType at =
  record
    [ ("center", at.center |> point)
    , ("radius", at.radius |> float)
    , ("fromAngle", at.fromAngle |> float)
    , ("toAngle", at.toAngle |> float)
    ]
