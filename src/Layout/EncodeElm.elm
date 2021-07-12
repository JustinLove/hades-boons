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
    , ("link", c.link |> List.map string |> list)
    , ("misc", c.misc |> bool)
    , ("shape", c.shape |> connectionType)
    ]

maybe : (a -> Expression) -> Maybe a -> Expression
maybe encoder m =
  case m of
    Just x -> construct "Just" [encoder x]
    Nothing -> val "Nothing"

bool : Bool -> Expression
bool b =
  if b then
    val "True"
  else
    val "False"

connectionType : ConnectionType -> Expression
connectionType ct =
  case ct of
    Arc at -> construct "Arc" [arcType at]
    Area boundaries -> construct "Area" [boundaries |> List.map boundary |> list]
    Dot p -> construct "Dot" [point p]
    Circle c r -> construct "Circle" [point c, float r]
    EllipticArc at -> construct "EllipticArc" [ellipticArcType at]
    Line a b -> construct "Line" [point a, point b]
    PolyLine points -> construct "PolyLine" [points |> List.map point |> list]

arcType : ArcType -> Expression
arcType at =
  record
    [ ("center", at.center |> point)
    , ("radius", at.radius |> float)
    , ("fromAngle", at.fromAngle |> float)
    , ("toAngle", at.toAngle |> float)
    , ("winding", at.winding |> winding)
    ]

ellipticArcType : EllipticArcType -> Expression
ellipticArcType at =
  record
    [ ("center", at.center |> point)
    , ("majorAxis", at.majorAxis |> point)
    , ("minorRatio", at.minorRatio |> float)
    , ("fromAngle", at.fromAngle |> float)
    , ("toAngle", at.toAngle |> float)
    ]

winding : Winding -> Expression
winding w =
  case w of
    Clockwise -> val "Clockwise"
    Counterclockwise -> val "Counterclockwise"

boundary : Boundary -> Expression
boundary bound =
  case bound of
    BoundaryArc at -> construct "BoundaryArc" [arcType at]
    BoundaryLine a b -> construct "BoundaryLine" [point a, point b]
