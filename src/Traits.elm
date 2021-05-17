module Traits exposing
  ( TraitId
  , Traits
  , makeTraits
  , empty
  , God(..)
  , BoonType(..)
  , GodData
  , GodDataRecord
  , godData
  , Trait
  , Requirements(..)
  , linkableGods
  , allGods
  , dataGod
  , dataName
  , dataLootColor
  , godName
  , duoBoons
  , basicBoons
  , identifyBoons
  , isAvailable
  )

import Color exposing (Color)
import Set exposing (Set)

type alias TraitId = String

type Traits = Traits (List GodData)

makeTraits = Traits
empty = Traits []

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

type GodData = GodData GodDataRecord

type alias GodDataRecord =
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

linkableGods : Traits -> List GodData
linkableGods (Traits gods) =
  List.drop 1 gods

allGods : Traits -> List GodData
allGods (Traits gods) = gods

godData : GodDataRecord -> GodData
godData = GodData

dataGod : GodData -> God
dataGod (GodData data) = data.god

dataName : GodData -> String
dataName (GodData data) = godName data.god

dataLootColor : GodData -> Color
dataLootColor (GodData data) = data.lootColor

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

basicBoons : GodData -> List Trait
basicBoons data =
  data
    |> godTraits
    |> List.filter isBasicBoon

duoBoons : Traits -> List Trait
duoBoons traits =
  traits
    |> allGods
    |> List.concatMap duoGodBoons

duoGodBoons : GodData -> List Trait
duoGodBoons (GodData data) =
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
    |> allGods
    |> List.map tagBasicGodBoons
    |> makeTraits

tagBasicGodBoons : GodData -> GodData
tagBasicGodBoons (GodData data) =
  GodData { data | traits = data.traits
    |> List.map (tagBoonAs (BasicBoon data.god))
  }

tagBoonAs : BoonType -> Trait -> Trait
tagBoonAs boonType trait =
  { trait | boonType = boonType }

tagLinkedBoons : Traits -> Traits
tagLinkedBoons traits =
  traits
    |> allGods
    |> List.map (tagLinkedGodBoons traits)
    |> makeTraits

tagLinkedGodBoons : Traits -> GodData -> GodData
tagLinkedGodBoons traits (GodData god) =
  GodData { god | linkedUpgrades = god.linkedUpgrades
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
findBoon traits id =
  traits
    |> allGods
    |> List.concatMap godTraits
    |> List.filter (hasId id)
    |> List.head

godTraits : GodData -> List Trait
godTraits (GodData data) =
  List.append data.traits data.linkedUpgrades

hasId : TraitId -> Trait -> Bool
hasId id trait =
  trait.trait == id

isAvailable : Traits -> Set TraitId -> TraitId -> Bool
isAvailable gods activeTraits id =
  findBoon gods id
    |> Maybe.map (\{requirements} ->
      case requirements of
        None -> True
        OneOf set -> Set.intersect activeTraits set |> Set.isEmpty |> not
        OneFromEachSet list ->
          list
            |> List.all (Set.intersect activeTraits >> Set.isEmpty >> not)
      )
    |> Maybe.withDefault False
