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
  , isAvailable
  )

import Color exposing (Color)
import Set exposing (Set)

type alias TraitId = String

type Traits = Traits
  { gods : List GodData
  , duos : List Trait
  }

empty : Traits
empty = Traits {gods = [], duos = []}

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
linkableGods (Traits {gods}) =
  List.drop 1 gods

allGods : Traits -> List GodData
allGods (Traits {gods}) = gods

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
duoBoons (Traits {duos}) = duos

duoGodBoons : GodData -> List Trait
duoGodBoons (GodData data) =
  data.traits
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

makeTraits : List GodData -> Traits
makeTraits gods =
  gods
    |> tagLinkedBoons -- basics
    |> tagLinkedBoons -- level 1 requirements
    |> tagLinkedBoons -- propigate to legenedaries
    |> separateDuos

tagLinkedBoons : List GodData -> List GodData
tagLinkedBoons gods =
  gods
    |> List.map (tagLinkedGodBoons gods)

tagLinkedGodBoons : List GodData -> GodData -> GodData
tagLinkedGodBoons gods (GodData god) =
  GodData { god | traits =
    god.traits
      |> List.map (tagLinkedBoon gods god.god)
  }

tagLinkedBoon : List GodData -> God -> Trait -> Trait
tagLinkedBoon gods god trait =
  { trait | boonType = boonTypeFromRequirements gods god trait.requirements }

boonTypeFromRequirements : List GodData -> God -> Requirements -> BoonType
boonTypeFromRequirements gods god requirements =
  case requirements of
    None -> BasicBoon god
    OneOf set -> boonTypeFromGroup (godOfSet gods set)
    OneFromEachSet list ->
      list
        |> List.map (godOfSet gods)
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

godOfSet : List GodData -> Set TraitId -> GodsInGroup
godOfSet gods set =
  set |> Set.foldr (godAccumulator gods) Empty

godAccumulator : List GodData -> TraitId -> GodsInGroup -> GodsInGroup
godAccumulator gods id group =
  case (findBoonType gods id, group) of
    (_, Many) -> Many
    (UnknownBoon, b) -> b
    (BasicBoon a, Empty) -> One a
    (BasicBoon a, One b) -> if a == b then One a else Many
    (DuoBoon _ _, _) -> Many

findBoonType : List GodData -> TraitId -> BoonType
findBoonType gods id =
  case findGodBoon gods id of
    Just {boonType} -> boonType
    Nothing -> UnknownBoon

findGodBoon : List GodData -> TraitId -> Maybe Trait
findGodBoon gods id =
  gods
    |> List.concatMap godTraits
    |> List.filter (hasId id)
    |> List.head

separateDuos : List GodData -> Traits
separateDuos gods =
  let
    (basicGods, duos) =
      gods
        |> List.map extractDuos
        |> List.unzip
  in
    Traits { gods = basicGods, duos = List.concat duos }

extractDuos : GodData -> (GodData, List Trait)
extractDuos (GodData data) =
  let
    (duos, basic) = List.partition isDuoBoon data.traits
  in
    (GodData {data | traits = basic}, duos)

godTraits : GodData -> List Trait
godTraits (GodData data) =
  data.traits

hasId : TraitId -> Trait -> Bool
hasId id trait =
  trait.trait == id

findBoon : Traits -> TraitId -> Maybe Trait
findBoon traits id =
  traits
    |> allGods
    |> List.concatMap godTraits
    |> List.append (duoBoons traits)
    |> List.filter (hasId id)
    |> List.head

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
