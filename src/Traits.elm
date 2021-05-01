module Traits exposing
  ( TraitId
  , Traits
  , God(..)
  , BoonType(..)
  , GodData
  , Trait
  , Requirements(..)
  , godName
  , duoBoons
  , basicBoonsOf
  , basicBoons
  , identifyBoons
  )

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

type BoonType
  = UnknownBoon
  | BasicBoon God
  | DuoBoon God God

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
  , boonType : BoonType
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

basicBoonsOf : God -> Traits -> List Trait
basicBoonsOf god traits =
  traits
    |> List.concatMap (\data ->
      if data.god == god then
        basicBoons data
      else
        []
      )

basicBoons : GodData -> List Trait
basicBoons data =
  data
    |> godTraits
    |> List.filter isBasicBoon

duoBoons : Traits -> List Trait
duoBoons traits =
  traits
    |> List.concatMap duoGodBoons

duoGodBoons : GodData -> List Trait
duoGodBoons data =
  data.linkedUpgrades
    |> List.filter isDuoBoon

isBasicBoon : Trait -> Bool
isBasicBoon {boonType} =
  case boonType of
    UnknownBoon -> False
    BasicBoon _ -> True
    DuoBoon _ _ -> False

isDuoBoon : Trait -> Bool
isDuoBoon {boonType} =
  case boonType of
    UnknownBoon -> False
    BasicBoon _ -> False
    DuoBoon _ _ -> True

identifyBoons : Traits -> Traits
identifyBoons traits =
  traits
    |> tagBasicBoons
    |> tagLinkedBoons
    |> tagLinkedBoons -- propigate to legenedaries

tagBasicBoons : Traits -> Traits
tagBasicBoons traits =
  traits
    |> List.map tagBasicGodBoons

tagBasicGodBoons : GodData -> GodData
tagBasicGodBoons data =
  { data | traits = data.traits
    |> List.map (tagBoonAs (BasicBoon data.god))
  }

tagBoonAs : BoonType -> Trait -> Trait
tagBoonAs boonType trait =
  { trait | boonType = boonType }

tagLinkedBoons : Traits -> Traits
tagLinkedBoons traits =
  traits
    |> List.map (tagLinkedGodBoons traits)

tagLinkedGodBoons : Traits -> GodData -> GodData
tagLinkedGodBoons traits god =
  { god | linkedUpgrades = god.linkedUpgrades
    |> List.map (tagLinkedBoon traits)
  }

tagLinkedBoon : Traits -> Trait -> Trait
tagLinkedBoon traits trait =
  { trait | boonType = boonTypeFromRequirements traits trait.requirements }

boonTypeFromRequirements : Traits -> Requirements -> BoonType
boonTypeFromRequirements traits requirements =
  case requirements of
    None -> UnknownBoon
    OneOf set -> boonTypeFromGroup (godOfSet traits set)
    OneFromEachSet list ->
      list
        |> List.map (godOfSet traits)
        |> List.foldr oneFromEachSetAccumulator UnknownBoon

type GodsInGroup
  = Empty
  | One God
  | Many

boonTypeFromGroup : GodsInGroup -> BoonType
boonTypeFromGroup group =
  case group of
    Empty -> UnknownBoon
    One god -> BasicBoon god
    Many -> UnknownBoon

oneFromEachSetAccumulator : GodsInGroup -> BoonType -> BoonType
oneFromEachSetAccumulator group boonType =
  case (boonType, group) of
    (_, Many) -> boonType
    (_, Empty) -> boonType
    (UnknownBoon, One g) -> BasicBoon g
    (BasicBoon a, One g) ->
      if a == g then
        BasicBoon a
      else
        DuoBoon a g
    (DuoBoon a b, One g) -> 
      if a == g || b == g then
        DuoBoon a b
      else
        UnknownBoon

godOfSet : Traits -> Set TraitId -> GodsInGroup
godOfSet traits set =
  set |> Set.foldr (godAccumulator traits) Empty

godAccumulator : Traits -> TraitId -> GodsInGroup -> GodsInGroup
godAccumulator traits id group =
  case (findBoonType traits id, group) of
    (_, Many) -> Many
    (UnknownBoon, b) -> b
    (BasicBoon a, Empty) -> One a
    (BasicBoon a, One b) -> if a == b then One a else Many
    (DuoBoon _ _, _) -> Many

findBoonType : Traits -> TraitId -> BoonType
findBoonType traits id =
  case findBoon traits id of
    Just {boonType} -> boonType
    Nothing -> UnknownBoon

findBoon : Traits -> TraitId -> Maybe Trait
findBoon gods id =
  gods
    |> List.concatMap godTraits
    |> List.filter (hasId id)
    |> List.head

godTraits : GodData -> List Trait
godTraits data =
  List.append data.traits data.linkedUpgrades

hasId : TraitId -> Trait -> Bool
hasId id trait =
  trait.trait == id
