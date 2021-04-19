module BoonChart exposing (BoonChart, boonChart)

import Traits exposing (..)

import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render
import Collage.Text as Text
import Color exposing (Color)
import Html exposing (Html)
import Svg
import Svg.Attributes

type alias BoonChart =
  { traits : Traits
  }

size = 500
width = size
height = size

tau = pi*2


godOrder =
  [ "AphroditeUpgrade"
  , "AresUpgrade"
  , "DemeterUpgrade"
  , "DionysusUpgrade"
  , "PoseidonUpgrade"
  , "AthenaUpgrade"
  , "ArtemisUpgrade"
  , "ZeusUpgrade"
  ]

boonChart : List (Svg.Attribute msg) -> BoonChart -> Html msg
boonChart attributes {traits} =
  [ circle 0.45
      |> outlined (solid 0.01 (uniform Color.white))
  , gods traits
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

gods : Traits -> Collage msg
gods traits =
  let
    main = List.drop 1 traits
    count = List.length main
    factor = -tau / (toFloat count)
  in
    main
      |> List.indexedMap (\i data ->
        god data
          |> shiftY 0.35
          |> List.singleton
          |> group
          |> rotate ((toFloat i) * factor)
      )
      |> stack

god : God -> Collage msg
god data =
  Text.fromString data.name
    |> Text.color data.lootColor
    |> Text.size 100
    |> rendered
    |> scale 0.0002
