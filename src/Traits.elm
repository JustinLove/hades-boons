module Traits exposing
  ( TraitId
  , SlotId
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
  , BoonStatus(..)
  , linkableGods
  , allGods
  , dataGod
  , dataName
  , dataLootColor
  , dataColor
  , dataLayout
  , godName
  , godIcon
  , duoBoons
  , basicBoons
  , boonStatus
  , traitStatus
  , addLayout
  , calculateActiveLayoutGroups
  , calculateActiveDuoSets
  , calculateActiveSlots
  )

import Layout exposing (Layout)

import Color exposing (Color)
import Dict exposing (Dict)
import Set exposing (Set)

type alias TraitId = String
type alias SlotId = String

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
  , slot : Maybe SlotId
  , requiredSlottedTrait : Maybe SlotId
  , requirements : Requirements
  , boonType : BoonType
  }

type Requirements
  = None
  | OneOf (Set TraitId)
  | OneFromEachSet (List (Set TraitId))

type BoonStatus
  = Active
  | Available
  | Excluded
  | Unavailable

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


godIcon : God -> String
godIcon god =
  "GUI/Screens/BoonSelectSymbols/" ++ (godName god) ++ ".png"

basicBoons : GodData -> List Trait
basicBoons data =
  data
    |> godTraits
    |> List.filter isBasicBoon

duoBoons : Traits -> List Trait
duoBoons (Traits {duos}) = duos

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

boonStatus : Set TraitId -> Set SlotId -> Set TraitId -> Trait -> BoonStatus
boonStatus activeTraits activeSlots excludedTraits trait =
  if Set.member trait.trait activeTraits then
    Active
  else
    if Set.member trait.trait excludedTraits then
      Excluded
    else if boonHasRequiredSlottedTrait activeSlots trait && boonMeetsRequirements activeTraits trait then
      Available
    else
      Unavailable

slotFilled : Set SlotId -> Trait -> Bool
slotFilled activeSlots trait =
  case trait.slot of
    Just slot ->
      if Set.member slot activeSlots then
        True
      else
        False
    Nothing ->
      False

boonHasRequiredSlottedTrait : Set SlotId -> Trait -> Bool
boonHasRequiredSlottedTrait activeSlots trait =
  case trait.requiredSlottedTrait of
    Just slot ->
      if Set.member slot activeSlots then
        True
      else
        False
    Nothing ->
      True

boonMeetsRequirements : Set TraitId -> Trait -> Bool
boonMeetsRequirements activeTraits trait =
  case trait.requirements of
    None ->
      True
    OneOf set ->
      if Set.intersect activeTraits set |> Set.isEmpty |> not then
        True
      else
        False
    OneFromEachSet list ->
      if list |> List.all (Set.intersect activeTraits >> Set.isEmpty >> not) then
        True
      else
        False

boonExcludedByRequirements : Set TraitId -> Trait -> Bool
boonExcludedByRequirements excludedTraits trait =
  case trait.requirements of
    None ->
      False
    OneOf set ->
      if Set.diff set excludedTraits |> Set.isEmpty then
        True
      else
        False
    OneFromEachSet list ->
      if list |> List.any (\set -> Set.diff set excludedTraits |> Set.isEmpty) then
        True
      else
        False

traitStatus : Set TraitId -> Traits -> Dict TraitId BoonStatus
traitStatus activeTraits traits =
  let
    activeSlots = calculateActiveSlots activeTraits traits
    excludedTraits = calculateExcludedTraits activeTraits activeSlots traits
  in
  allTraits traits
    |> List.map (\trait -> (trait.trait, boonStatus activeTraits activeSlots excludedTraits trait))
    |> Dict.fromList

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

calculateActiveLayoutGroups : Set TraitId -> List GodData -> List (Set Layout.GroupId)
calculateActiveLayoutGroups activeTraits gods =
  gods
    |> List.map
      (\(GodData {layout}) -> Layout.calculateActiveGroups activeTraits layout)

calculateActiveDuoSets : List GodData -> Set TraitId -> List Trait -> Set Layout.GroupId
calculateActiveDuoSets gods activeTraits duos =
  duos
    |> List.foldl
      (\trait active -> calculateActiveSets gods activeTraits trait |> Set.union active)
      Set.empty

calculateActiveSets : List GodData -> Set TraitId -> Trait -> Set Layout.GroupId
calculateActiveSets gods activeTraits trait =
  case trait.requirements of
    None ->
      Set.empty
    OneOf set ->
        if Set.intersect activeTraits set |> Set.isEmpty |> not then
          activeRequirementSet gods trait set
            |> Maybe.map Set.singleton
            |> Maybe.withDefault Set.empty
        else
          Set.empty
    OneFromEachSet list ->
      list
        |> List.filter (Set.intersect activeTraits >> Set.isEmpty >> not)
        |> List.filterMap (activeRequirementSet gods trait)
        |> Set.fromList

activeRequirementSet : List GodData -> Trait -> Set TraitId -> Maybe Layout.GroupId
activeRequirementSet gods {trait} set =
  case godOfSet gods set of
    Empty -> Nothing
    One god -> Just ((godName god) ++ trait)
    Many -> Nothing

calculateActiveSlots : Set TraitId -> Traits -> Set SlotId
calculateActiveSlots activeTraits (Traits {gods}) =
  gods
    |> List.concatMap godTraits
    |> List.filter (\{trait} -> Set.member trait activeTraits)
    |> List.filterMap .slot
    |> Set.fromList

calculateExcludedTraits : Set TraitId -> Set SlotId -> Traits -> Set TraitId
calculateExcludedTraits activeTraits activeSlots (Traits {gods, duos}) =
  let basics = gods |> List.concatMap godTraits in
  calculateExcludedSlotTraits activeTraits activeSlots gods
    |> calculateExcludedDerivedTraits basics -- first requirments
    |> calculateExcludedDerivedTraits basics -- legendary
    |> calculateExcludedDerivedTraits duos -- duos

calculateExcludedSlotTraits : Set TraitId -> Set SlotId -> List GodData -> Set TraitId
calculateExcludedSlotTraits activeTraits activeSlots gods =
  gods
    |> List.concatMap godTraits
    |> List.filter (\{trait} -> not <| Set.member trait activeTraits)
    |> List.filter (\trait -> slotFilled activeSlots trait)
    |> List.map .trait
    |> Set.fromList

calculateExcludedDerivedTraits : List Trait -> Set TraitId -> Set TraitId
calculateExcludedDerivedTraits traits excludedTraits =
  traits
    |> List.filter (\trait -> boonExcludedByRequirements excludedTraits trait)
    |> List.map .trait
    |> Set.fromList
    |> Set.union excludedTraits
