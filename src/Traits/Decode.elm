module Traits.Decode exposing (traits)

import Layout
import Traits exposing (..)

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
      map3 GodDataRecord
        (succeed godTag)
        (allTraits godTag)
        (succeed Layout.empty)
      )

allTraits : God -> Decoder (List Trait)
allTraits godTag =
  map (List.append (trinkets godTag))
    (dataTraits godTag)

dataTraits : God -> Decoder (List Trait)
dataTraits godTag =
  map2 List.append
    (field "Traits" (list (trait godTag)))
    (field "LinkedUpgrades" (list (trait godTag)))

-- see also: Traits.miscBoons
trinkets : God -> List Trait
trinkets godTag =
  case godTag of
    Poseidon ->
      [ { icon = "GUI/Screens/AwardMenu/conch_shell_17.png"
        , trait = "ForcePoseidonBoonTrait"
        , name = "Conch Shell"
        , slot = Just "Keepsake"
        , requiredSlottedTrait = Nothing
        , requiredMetaUpgradeSelected = Nothing
        , requiredFalseTraits = Set.empty
        , requirements = None
        , boonType = BasicBoon Poseidon
        }
      ]
    Hermes ->
      [ { icon = "GUI/Screens/AwardMenu/feather.png"
        , trait = "FastClearDodgeBonusTrait"
        , name = "Lambent Plume"
        , slot = Just "Keepsake"
        , requiredSlottedTrait = Nothing
        , requiredMetaUpgradeSelected = Nothing
        , requiredFalseTraits = Set.empty
        , requirements = None
        , boonType = BasicBoon Hermes
        }
      ]
    _ ->
      []

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
  succeed Trait
    |> map2 (|>) (field "icon" string)
    |> map2 (|>) (field "trait" string)
    |> map2 (|>) (field "name" string)
    |> map2 (|>) (maybe (field "slot" string))
    |> map2 (|>) (maybe (field "RequiredSlottedTrait" string))
    |> map2 (|>) (maybe (field "RequiredMetaUpgradeSelected" string))
    |> map2 (|>) (oneOf
      [ (field "RequiredFalseTraits" (set string))
      , succeed Set.empty
      ]
    )
    |> map2 (|>) requirements
    |> map2 (|>) (succeed (BasicBoon godTag))

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
