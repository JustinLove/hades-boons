module Traits.Decode exposing (traits)

import Traits exposing (..)

import Color exposing (Color)
import Json.Decode exposing (..)
import Set exposing (Set)

traits : Decoder (List God)
traits =
  list god

god : Decoder God
god =
  map5 God
    (field "Name" string)
    (field "LootColor" color)
    (field "Color" color)
    (field "Traits" (list trait))
    (field "LinkedUpgrades" (list trait))

trait : Decoder Trait
trait =
  map4 Trait
    (field "icon" string)
    (field "trait" string)
    (field "name" string)
    requirements

requirements : Decoder Requirements
requirements =
  oneOf
    [ (field "OneOf" (set string)) |> map Traits.OneOf
    , (field "OneFromEachSet" (list (set string))) |> map OneFromEachSet
    , succeed None
    ]

set : Decoder comparable -> Decoder (Set comparable)
set decoder =
  list decoder |> map Set.fromList

color : Decoder Color
color =
  map3 Color.rgb255
    (index 0 int)
    (index 1 int)
    (index 2 int)
