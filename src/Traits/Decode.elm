module Traits.Decode exposing (traits)

import Layout
import Traits exposing (..)

import Color exposing (Color)
import Json.Decode exposing (..)
import Set exposing (Set)

traits : Decoder Traits
traits =
  list godData
    |> map makeTraits

godData : Decoder GodData
godData =
  map Traits.godData godDataRecord

godDataRecord : Decoder GodDataRecord
godDataRecord =
  (field "Name" god)
    |> andThen (\godTag ->
      map5 GodDataRecord
        (succeed godTag)
        (field "LootColor" color)
        (field "Color" color)
        (allTraits godTag)
        (succeed Layout.empty)
      )

allTraits : God -> Decoder (List Trait)
allTraits godTag =
  map2 List.append
    (field "Traits" (list (trait godTag)))
    (field "LinkedUpgrades" (list (trait godTag)))

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

trait : God -> Decoder Trait
trait godTag =
  map5 Trait
    (field "icon" string)
    (field "trait" string)
    (field "name" string)
    requirements
    (succeed (BasicBoon godTag))

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
