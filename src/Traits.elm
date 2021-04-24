module Traits exposing (..)

import Color exposing (Color)
import Set exposing (Set)

type alias TraitId = String

type alias Traits = List GodData

type God
  = Hermes
  | Aphrodite
  | Ares
  | Demeter
  | Dionysus
  | Poseidon
  | Athena
  | Artemis
  | Zeus

type alias GodData =
  { god : God
  , lootColor : Color
  , color : Color
  , traits : List Trait
  , linkedUpgrades : List Trait
  }

type alias Trait =
  { icon : String
  , trait : TraitId
  , name : String
  , requirements : Requirements
  }

type Requirements
  = None
  | OneOf (Set TraitId)
  | OneFromEachSet (List (Set TraitId))

godName : God -> String
godName god =
  case god of
    Hermes -> "Hermes"
    Aphrodite -> "Aphrodite"
    Ares -> "Ares"
    Demeter -> "Demeter"
    Dionysus -> "Dionysus"
    Poseidon -> "Poseidon"
    Athena -> "Athena"
    Artemis -> "Artemis"
    Zeus -> "Zeus"
