module Traits exposing (..)

import Color exposing (Color)
import Set exposing (Set)

type alias TraitId = String

type alias Traits = List God

type alias God =
  { name : String
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
