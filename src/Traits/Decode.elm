module Traits.Decode exposing (traits)

import Traits exposing (..)

import Color exposing (Color)
import Json.Decode exposing (..)
import Set exposing (Set)

traits : Decoder (List GodData)
traits =
  list godData

godData : Decoder GodData
godData =
  map5 GodData
    (field "Name" god)
    (field "LootColor" color)
    (field "Color" color)
    (field "Traits" (list trait))
    (field "LinkedUpgrades" (list trait))

god : Decoder God
god =
  string
    |> andThen (\upgrade ->
      case upgrade of
        "HermesUpgrade" -> succeed Hermes
        "AphroditeUpgrade" -> succeed Aphrodite
        "AresUpgrade" -> succeed Ares
        "DemeterUpgrade" -> succeed Demeter
        "DionysusUpgrade" -> succeed Dionysus
        "PoseidonUpgrade" -> succeed Poseidon
        "AthenaUpgrade" -> succeed Athena
        "ArtemisUpgrade" -> succeed Artemis
        "ZeusUpgrade" -> succeed Zeus
        _ -> fail ("Unknown god " ++ upgrade)
      )

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
