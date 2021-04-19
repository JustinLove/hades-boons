module Traits.Decode exposing (traits)

import Traits exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Set exposing (Set)

traits : Decoder (Dict TraitId God)
traits =
  dict god

god : Decoder God
god =
  map4 God
    (field "LootColor" string)
    (field "Color" string)
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
