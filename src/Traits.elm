module Traits exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

type alias TraitId = String

type alias Traits = Dict String God

type alias God =
  { lootColor : String
  , color : String
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
