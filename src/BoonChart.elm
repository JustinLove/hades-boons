module BoonChart exposing (BoonChart, boonChart)

import Collage exposing (..)
import Collage.Render
import Color exposing (Color)
import Html exposing (Html)
import Svg
--import Svg.Attributes

type alias BoonChart =
  {
  }

boonChart : List (Svg.Attribute msg) -> BoonChart -> Html msg
boonChart attributes {} =
  circle 100
    |> outlined (solid 10 (uniform Color.white))
    |> Collage.Render.svg
