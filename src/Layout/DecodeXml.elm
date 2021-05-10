module Layout.DecodeXml exposing
  ( layout
  , placement
  , point
  )

import Layout exposing (..)
import Xml.Decode exposing (..)

layout : Decoder Layout
layout =
  succeed Layout
    |> map2 (|>) (path ["diagram", "mxGraphModel", "root", "mxCell"] (leakyList placement))
    |> map2 (|>) (succeed [])

placement : Decoder Placement
placement =
  succeed Placement
    |> map2 (|>) (stringAttr "value")
    |> requiredPath ["mxGeometry"] (single point)

point : Decoder (Float, Float)
point =
  map2 Tuple.pair
    (floatAttr "x")
    (floatAttr "y")
