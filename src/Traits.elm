module Traits exposing
  ( TraitId
  , Traits
  , makeTraits
  , loadPreprocessedGodsAndDuoBoons
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
  , dataColor
  , dataLayout
  , godName
  , duoBoons
  , basicBoons
  , isAvailable
  , addLayout
  , calculateActiveGroups
  )

import Layout exposing (Layout)

import Color exposing (Color)
import Dict exposing (Dict)
import Set exposing (Set)

type alias TraitId = String

type Traits = Traits
  { gods : List GodData
  , duos : List Trait
  , requirementsCache : Dict TraitId Requirements
  }

empty : Traits
empty = Traits {gods = [], duos = [], requirementsCache = Dict.empty}

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
  = BasicBoon God
  | DuoBoon God God

type GodData = GodData GodDataRecord

type alias GodDataRecord =
  { god : God
  , lootColor : Color
  , color : Color
  , traits : List Trait
  , layout : Layout
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

dataColor : GodData -> Color
dataColor (GodData data) = data.color

dataLayout : GodData -> Layout
dataLayout (GodData data) = data.layout

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
    BasicBoon _ -> True
    DuoBoon _ _ -> False

isDuoBoon : Trait -> Bool
isDuoBoon {boonType} =
  case boonType of
    BasicBoon _ -> False
    DuoBoon _ _ -> True

makeTraits : List GodData -> Traits
makeTraits gods =
  gods
    |> tagLinkedBoons -- propagate to duos
    |> separateDuos

loadPreprocessedGodsAndDuoBoons : List GodData -> List Trait -> Traits
loadPreprocessedGodsAndDuoBoons basicGods duos =
  Traits
    { gods = basicGods
    , duos = duos
    , requirementsCache =
      basicGods
        |> List.concatMap godTraits
        |> List.append duos
        |> List.map (\{trait, requirements} -> (trait, requirements))
        |> Dict.fromList
    }

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
    OneOf set -> boonTypeFromGroup god (godOfSet gods set)
    OneFromEachSet list ->
      list
        |> List.map (godOfSet gods)
        |> List.foldr oneFromEachSetAccumulator (BasicBoon god)

type GodsInGroup
  = Empty
  | One God
  | Many

boonTypeFromGroup : God -> GodsInGroup -> BoonType
boonTypeFromGroup default group =
  case group of
    Empty -> BasicBoon default
    One god -> BasicBoon god
    Many -> BasicBoon default

oneFromEachSetAccumulator : GodsInGroup -> BoonType -> BoonType
oneFromEachSetAccumulator group boonType =
  case (boonType, group) of
    (_, Many) -> boonType
    (_, Empty) -> boonType
    (BasicBoon a, One g) ->
      if a == g then
        BasicBoon a
      else
        DuoBoon a g
    (DuoBoon a b, One g) -> 
      if a == g || b == g then
        DuoBoon a b
      else
        Debug.todo "too many gods"
        -- no way to punt
        --boonType

godOfSet : List GodData -> Set TraitId -> GodsInGroup
godOfSet gods set =
  set |> Set.foldr (godAccumulator gods) Empty

godAccumulator : List GodData -> TraitId -> GodsInGroup -> GodsInGroup
godAccumulator gods id group =
  case (findBoonType gods id, group) of
    (_, Many) -> Many
    (Nothing, _) -> group
    (Just (BasicBoon a), Empty) -> One a
    (Just (BasicBoon a), One b) -> if a == b then One a else Many
    (Just (DuoBoon _ _), _) -> Many

findBoonType : List GodData -> TraitId -> Maybe BoonType
findBoonType gods id =
  findGodBoon gods id
    |> Maybe.map .boonType

findGodBoon : List GodData -> TraitId -> Maybe Trait
findGodBoon gods id =
  gods
    |> List.concatMap godTraits
    |> List.filter (hasId id)
    |> List.head

separateDuos : List GodData -> Traits
separateDuos gods =
  let
    (basicGods, listOfDuos) =
      gods
        |> List.map extractDuos
        |> List.unzip
    duos = List.concat listOfDuos
  in
    loadPreprocessedGodsAndDuoBoons basicGods duos

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

allTraits : Traits -> List Trait
allTraits traits =
  traits
    |> allGods
    |> List.concatMap godTraits
    |> List.append (duoBoons traits)

findBoon : Traits -> TraitId -> Maybe Trait
findBoon traits id =
  traits
    |> allTraits
    |> List.filter (hasId id)
    |> List.head

isAvailable : Traits -> Set TraitId -> TraitId -> Bool
isAvailable (Traits {requirementsCache}) activeTraits id =
  Dict.get id requirementsCache
    |> Maybe.map (\requirements ->
      case requirements of
        None -> True
        OneOf set -> Set.intersect activeTraits set |> Set.isEmpty |> not
        OneFromEachSet list ->
          list
            |> List.all (Set.intersect activeTraits >> Set.isEmpty >> not)
      )
    |> Maybe.withDefault False

addLayout : God -> Layout -> Traits -> Traits
addLayout god layout (Traits traits) =
  Traits { traits
  | gods = List.map (\goddata ->
      case goddata of
        GodData data ->
          if data.god == god then
            GodData { data | layout = layout }
          else
            goddata
    ) traits.gods
  }

calculateActiveGroups : Set TraitId -> Traits -> Set Layout.GroupId
calculateActiveGroups activeTraits (Traits {gods}) =
  gods
    |> List.foldl
      (\(GodData {layout}) active -> Layout.calculateActiveGroups activeTraits layout |> Set.union active)
      Set.empty
