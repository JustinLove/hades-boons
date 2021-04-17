module BoonChart exposing (BoonChart, boonChart)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render
import Color exposing (Color)
import Html exposing (Html)
import Svg
import Svg.Attributes

type alias BoonChart =
  {
  }

size = 1000
width = size
height = size

boonChart : List (Svg.Attribute msg) -> BoonChart -> Html msg
boonChart attributes {} =
  [ circle 0.45
      |> outlined (solid 0.01 (uniform Color.white))
  , rectangle 1 1
      --|> outlined (solid 0.005 (uniform Color.white))
      |> outlined (solid 0.005 (transparent))
  ]
    |> stack
    |> align topLeft
    |> List.singleton
    |> group
    |> scale size
    |> debug
    |> Collage.Render.svgExplicit attributes
