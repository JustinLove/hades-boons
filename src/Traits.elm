module Traits exposing
  ( TraitId
  , SlotId
  , Traits
  , makeTraits
  , loadPreprocessedGodsAndDuoBoons
  , empty
  , God(..)
  , BoonType(..)
  , Frame(..)
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
  , dataLayout
  , godName
  , godIcon
  , godColor
  , duoBoons
  , miscBoons
  , boonsForSlot
  , isSlot
  , slots
  , iconForSlot
  , nameForSlot
  , boonsOf
  , basicBoons
  , dataBoon
  , allTraits
  , findBoon
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
  , misc : List Trait
  }

empty : Traits
empty = Traits {gods = [], duos = [], misc = []}

type God
  = Hades
  | Charon
  | Nyx
  | Hermes
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
  | Keepsake

type Frame
  = CommonFrame
  | DuoFrame
  | KeepsakeFrame
  | LegendaryFrame

type GodData = GodData GodDataRecord

type alias GodDataRecord =
  { god : God
  , traits : List Trait
  , layout : Layout
  }

type alias Trait =
  { icon : String
  , trait : TraitId
  , name : String
  , description : String
  , tooltipData : Dict String Float
  , slot : Maybe SlotId
  , requiredSlottedTrait : Maybe SlotId
  , requiredMetaUpgradeSelected : Maybe TraitId
  , requiredFalseTraits : Set TraitId
  , requirements : Requirements
  , boonType : BoonType
  , frame : Frame
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
dataLootColor (GodData data) = godColor data.god

dataLayout : GodData -> Layout
dataLayout (GodData data) = data.layout

godName : God -> String
godName god =
  case god of
    Charon -> "Charon"
    Nyx -> "Nyx"
    Hades -> "Hades"
    Hermes -> "Hermes"
    Aphrodite -> "Aphrodite"
    Ares -> "Ares"
    Demeter -> "Demeter"
    Dionysus -> "Dionysus"
    Poseidon -> "Poseidon"
    Athena -> "Athena"
    Artemis -> "Artemis"
    Zeus -> "Zeus"

godColor : God -> Color
godColor god =
  case god of
    Charon ->
      Color.fromRgba
        { red = 0.4
        , green = 0
        , blue = 0.706
        , alpha = 1
        }
    Nyx ->
      Color.fromRgba
        { red = 0.4
        , green = 0
        , blue = 0.706
        , alpha = 1
        }
    Hades ->
      Color.fromRgba
        { red = 0.776
        , green = 0
        , blue = 0
        , alpha = 1
        }
    Hermes ->
      Color.fromRgba
        { red = 1
        , green = 0.35294117647058826
        , blue = 0
        , alpha = 1
        }
    Aphrodite ->
      Color.fromRgba
        { red = 1
        , green = 0.19607843137254902
        , blue = 0.9411764705882353
        , alpha = 1
        }
    Ares ->
      Color.fromRgba
        { red = 1, green = 0.0784313725490196, blue = 0, alpha = 1 }
    Demeter ->
      Color.fromRgba
        { red = 0.631
        , green = 0.702
        , blue = 1
        , alpha = 1
        }
    Dionysus ->
      Color.fromRgba
        { red = 0.7843137254901961, green = 0, blue = 1, alpha = 1 }
    Poseidon ->
      Color.fromRgba
        { red = 0, green = 0.7843137254901961, blue = 1, alpha = 1 }
    Athena ->
      Color.fromRgba
        { red = 0.733
        , green = 0.69
        , blue = 0.373
        , alpha = 1
        }
    Artemis ->
      Color.fromRgba
        { red = 0.43137254901960786
        , green = 1
        , blue = 0
        , alpha = 1
        }
    Zeus ->
      Color.fromRgba
        { red = 1
        , green = 1
        , blue = 0.25098039215686274
        , alpha = 1
        }

godIcon : God -> String
godIcon god =
  "GUI/Screens/BoonSelectSymbols/" ++ (godName god) ++ ".png"

basicBoons : GodData -> List Trait
basicBoons data =
  data
    |> godTraits
    |> List.filter isBasicBoon

dataBoon : GodData -> TraitId -> Maybe Trait
dataBoon data id =
  data
    |> godTraits
    |> List.filter (hasId id)
    |> List.head

boonsForSlot : SlotId -> Traits -> List Trait
boonsForSlot slot (Traits {gods}) =
  gods
    |> List.concatMap basicBoons
    |> List.append miscBoons
    |> List.filter (isSlot slot)

isSlot : SlotId -> Trait -> Bool
isSlot target {slot} =
  slot == Just target

slots : List SlotId
slots =
  [ "Melee"
  , "Secondary"
  , "Ranged"
  , "Rush"
  , "Shout"
  ]

iconForSlot : SlotId -> String
iconForSlot slot =
  case slot of
    "Melee" -> "GUI/HUD/PrimaryBoons/SlotIcon_Attack_White.png"
    "Secondary" -> "GUI/HUD/PrimaryBoons/SlotIcon_Secondary_White.png"
    "Ranged" -> "GUI/HUD/PrimaryBoons/SlotIcon_Ranged_White.png"
    "Rush" -> "GUI/HUD/PrimaryBoons/SlotIcon_Dash_White.png"
    "Shout" -> "GUI/HUD/PrimaryBoons/SlotIcon_Wrath_White.png"
    _ -> "GUI/LockIcon/LockIcon0001.png"

nameForSlot : SlotId -> String
nameForSlot slot =
  case slot of
    "Melee" -> "Any Attack"
    "Secondary" -> "Any Secondary"
    "Ranged" -> "Any Ranged"
    "Rush" -> "Any Dash"
    "Shout" -> "Any Call"
    _ -> "Unknown Slot"

metaUpgrades : List String
metaUpgrades =
  [ "AmmoMetaUpgrade"
  , "ReloadAmmoMetaUpgrade"
  ]

boonsOf : God -> Traits -> List Trait
boonsOf target (Traits {gods}) =
  gods
    |> List.filter (\data -> dataGod data == target)
    |> List.concatMap basicBoons

duoBoons : Traits -> List Trait
duoBoons (Traits {duos}) = duos

singleBoons : Traits -> List Trait
singleBoons (Traits {gods}) =
  gods
    |> List.concatMap godTraits

isBasicBoon : Trait -> Bool
isBasicBoon {boonType} =
  case boonType of
    BasicBoon _ -> True
    DuoBoon _ _ -> False
    Keepsake -> True

isDuoBoon : Trait -> Bool
isDuoBoon {boonType} =
  case boonType of
    BasicBoon _ -> False
    DuoBoon _ _ -> True
    Keepsake -> False

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
    , misc = miscBoons
    }

-- see also: Traits.Decode.trinkets
miscBoons : List Trait
miscBoons =
  [ { icon = "GUI/Screens/WeaponEnchantmentIcons/bow_echantment_2.png"
    , trait = "BowLoadAmmoTrait"
    , name = "Aspect of Hera"
    , description = "Your {$Keywords.Cast} loads {!Icons.Ammo} into your next {$Keywords.Attack}, firing on impact."
    , tooltipData = Dict.empty
    , slot = Just "Weapon"
    , requiredSlottedTrait = Nothing
    , requiredMetaUpgradeSelected = Nothing
    , requiredFalseTraits = Set.empty
    , requirements = None
    , boonType = BasicBoon Charon
    , frame = CommonFrame
    }
  , { icon = "GUI/Screens/WeaponEnchantmentIcons/shield_enchantment_3.png"
    , trait = "ShieldLoadAmmoTrait"
    , name = "Aspect of Beowulf"
    , description = "You have {$Keywords.BeowulfAspect}, but take {#AltPenaltyFormat}{$TooltipData.TooltipDamageTaken:P} {#PreviousFormat}damage."
    , tooltipData = Dict.fromList [("TooltipDamageTaken",10)]
    , slot = Just "Weapon"
    , requiredSlottedTrait = Nothing
    , requiredMetaUpgradeSelected = Nothing
    , requiredFalseTraits = Set.empty
    , requirements = None
    , boonType = BasicBoon Charon
    , frame = CommonFrame
    }
  , { icon = "GUI/Screens/MirrorIcons/infernal soul.png"
    , trait = "AmmoMetaUpgrade"
    , name = "Infernal Soul"
    , description = "Raise your supply of {!Icons.Ammo} for your {#BoldFormatGraft}Cast{#PreviousFormat}, {#TooltipUpgradeFormat}+1 {#PreviousFormat}per rank."
    , tooltipData = Dict.empty
    , slot = Just "Soul"
    , requiredSlottedTrait = Nothing
    , requiredMetaUpgradeSelected = Nothing
    , requiredFalseTraits = Set.empty
    , requirements = None
    , boonType = BasicBoon Nyx
    , frame = KeepsakeFrame
    }
  , { icon = "GUI/Screens/MirrorBIcons/Stygian_Soul.png"
    , trait = "ReloadAmmoMetaUpgrade"
    , name = "Stygian Soul"
    , description = "Regenerate your {!Icons.Ammo} for your {#BoldFormatGraft}Cast {#PreviousFormat}{#ItalicFormat}(rather than pick it up){#PreviousFormat}, faster by {#TooltipUpgradeFormat}{$TempTextData.BaseValue} Sec. {#PreviousFormat}per rank."
    , tooltipData = Dict.fromList [("BaseValue",1)]
    , slot = Just "Soul"
    , requiredSlottedTrait = Nothing
    , requiredMetaUpgradeSelected = Nothing
    , requiredFalseTraits = Set.empty
    , requirements = None
    , boonType = BasicBoon Nyx
    , frame = KeepsakeFrame
    }
  , { icon = "GUI/Screens/AwardMenu/badge_23.png"
    , trait = "HadesShoutKeepsake"
    , name = "Sigil of the Dead"
    , description = "Your {$Keywords.WrathHades} becomes {#BoldFormatGraft}Hades' Aid{#PreviousFormat}, which briefly makes you {#BoldFormatGraft}{$Keywords.Invisible}{#PreviousFormat}; your {$Keywords.WrathGauge} starts {#UpgradeFormat}{$TooltipData.TooltipSuperGain}% {#PreviousFormat} full."
    , tooltipData = Dict.fromList [("TooltipSuperGain",15)]
    , slot = Just "Keepsake"
    , requiredSlottedTrait = Nothing
    , requiredMetaUpgradeSelected = Nothing
    , requiredFalseTraits = Set.fromList
      [ "AphroditeShoutTrait"
      , "AresShoutTrait"
      , "DemeterShoutTrait"
      , "DionysusShoutTrait"
      , "PoseidonShoutTrait"
      , "AthenaShoutTrait"
      , "ArtemisShoutTrait"
      , "ZeusShoutTrait"
      ]
    , requirements = None
    , boonType = BasicBoon Hades
    , frame = CommonFrame
    }
  , { icon = "GUI/Screens/BoonIcons/Hades_01_Large.png"
    , trait = "HadesShoutTrait"
    , name = "Hades Aid"
    , description = "Your {$Keywords.WrathHades} briefly makes you turn {$Keywords.Invisible}."
    , tooltipData = Dict.empty
    , slot = Just "Shout"
    , requiredSlottedTrait = Nothing
    , requiredMetaUpgradeSelected = Nothing
    , requiredFalseTraits = Set.empty
    , requirements = None
    , boonType = Keepsake
    , frame = KeepsakeFrame
    }
  ]

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
  case trait.boonType of
    Keepsake -> trait
    _ ->
      let boonType = boonTypeFromRequirements gods god trait.requirements in
      { trait
      | boonType = boonType
      , frame = case boonType of
        BasicBoon _ -> frameFromRequirements gods trait.requirements
        DuoBoon _ _ -> DuoFrame
        Keepsake -> KeepsakeFrame
      }

boonTypeFromRequirements : List GodData -> God -> Requirements -> BoonType
boonTypeFromRequirements gods god requirements =
  case requirements of
    None -> BasicBoon god
    OneOf set -> boonTypeFromGroup god (godOfSet gods set)
    OneFromEachSet list ->
      list
        |> List.map (godOfSet gods)
        |> List.foldr oneFromEachSetAccumulator (BasicBoon god)

frameFromRequirements : List GodData -> Requirements -> Frame
frameFromRequirements gods requirements =
  if legendaryRequirements gods requirements then
    LegendaryFrame
  else
    CommonFrame

legendaryRequirements : List GodData -> Requirements -> Bool
legendaryRequirements gods requirements =
  case requirements of
    None -> False
    OneOf set -> setHasRequirements gods set
    OneFromEachSet list -> True

setHasRequirements : List GodData -> Set TraitId -> Bool
setHasRequirements gods set =
  set
    |> Set.toList
    |> List.map (findBoonRequirements gods)
    |> List.any (\req ->
      req /= None
      && req /= OneOf (Set.singleton "ShieldLoadAmmoTrait")
      && req /= OneOf (Set.fromList ["BowLoadAmmoTrait", "ShieldLoadAmmoTrait"])
      )

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
        -- Debug.todo "too many gods"
        -- no way to punt
        boonType
    (Keepsake, _) ->
      boonType

godOfSet : List GodData -> Set TraitId -> GodsInGroup
godOfSet gods set =
  set |> Set.foldr (godAccumulator gods) Empty

godAccumulator : List GodData -> TraitId -> GodsInGroup -> GodsInGroup
godAccumulator gods id group =
  case (findBoonType gods id, group) of
    (_, Many) -> Many
    (Nothing, _) -> group
    (Just (Keepsake), _) -> group
    (Just (BasicBoon a), Empty) -> One a
    (Just (BasicBoon a), One b) -> if a == b then One a else Many
    (Just (DuoBoon _ _), _) -> Many

findBoonType : List GodData -> TraitId -> Maybe BoonType
findBoonType gods id =
  findGodBoon gods id
    |> Maybe.map .boonType

findBoonRequirements : List GodData -> TraitId -> Requirements
findBoonRequirements gods id =
  findGodBoon gods id
    |> Maybe.map .requirements
    |> Maybe.withDefault None

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
    |> List.append miscBoons
    |> List.filter (hasId id)
    |> List.head

boonStatus : Set TraitId -> Set SlotId -> Set TraitId -> Trait -> BoonStatus
boonStatus activeTraits activeSlots excludedTraits trait =
  if Set.member trait.trait activeTraits then
    Active
  else if Set.member trait.trait excludedTraits then
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

traitStatus : Set TraitId -> String -> Traits -> Dict TraitId BoonStatus
traitStatus activeTraits metaUpgrade traits =
  let
    activeSlots = calculateActiveSlots activeTraits traits
    excludedTraits = calculateExcludedTraits activeTraits activeSlots metaUpgrade traits
    statusOfTraits =
      allTraits traits
        |> List.map (\trait -> (trait.trait, boonStatus activeTraits activeSlots excludedTraits trait))
    statusOfSlots =
      slots
        |> List.map (\slot ->
            let id = "Any"++slot in
            ( id
            , if Set.member slot activeSlots then
                Active
              else
                Available
            )
          )
    statusOfMeta =
      metaUpgrades
        |> List.map (\id ->
            ( id
            , if id == metaUpgrade then
                Active
              else
                Excluded
            )
          )
  in
  statusOfTraits
    |> List.append statusOfSlots
    |> List.append statusOfMeta
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
    |> List.append miscBoons
    |> List.filter (\{trait} -> Set.member trait activeTraits)
    |> List.filterMap .slot
    |> Set.fromList

calculateExcludedTraits : Set TraitId -> Set SlotId -> String -> Traits -> Set TraitId
calculateExcludedTraits activeTraits activeSlots metaUpgrade (Traits {gods, duos} as traits) =
  let
    basics = gods |> List.concatMap godTraits
    all = allTraits traits
  in
  (Set.diff (Set.fromList ["ShieldLoadAmmoTrait", "BowLoadAmmoTrait"]) activeTraits)
    |> Set.union (calculateExcludedSlotTraits activeTraits activeSlots gods)
    |> Set.union (calculateExcludedIncompatibleTraits activeTraits all)
    |> Set.union (calculateExcludedMetaUpgradeTraits metaUpgrade all)
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

calculateExcludedIncompatibleTraits : Set TraitId -> List Trait -> Set TraitId
calculateExcludedIncompatibleTraits activeTraits traits =
  traits
    |> List.filter (\{trait} -> not <| Set.member trait activeTraits)
    |> List.filter (\{requiredFalseTraits} -> Set.intersect activeTraits requiredFalseTraits |> Set.isEmpty |> not)
    |> List.map .trait
    |> Set.fromList

calculateExcludedMetaUpgradeTraits : String -> List Trait -> Set TraitId
calculateExcludedMetaUpgradeTraits metaUpgrade traits =
  traits
    |> List.filter (\{requiredMetaUpgradeSelected} ->
      case requiredMetaUpgradeSelected of
        Just req -> metaUpgrade /= req
        Nothing -> False
      )
    |> List.map .trait
    |> Set.fromList

calculateExcludedDerivedTraits : List Trait -> Set TraitId -> Set TraitId
calculateExcludedDerivedTraits traits excludedTraits =
  traits
    |> List.filter (\trait -> boonExcludedByRequirements excludedTraits trait)
    |> List.map .trait
    |> Set.fromList
    |> Set.union excludedTraits
