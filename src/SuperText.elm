module SuperText exposing (..)

type SuperText
  = Text String
  | Format Format (List SuperText)
  | Icons Icon
  | Keywords Keyword
  | TooltipData Tooltip

type Format
  = AltPenalty
  | AltUpgrade
  | Bold
  | BoldGraft
  | Italic
  | PreviousFormat

type Icon
  = Ammo
  | Currency
  | Gem
  | GiftPoint
  | Health
  | HealthRestore
  | HealthUp
  | KEnemyHealth
  | MetaPoint

type Keyword = Keyword String

keywords : List String
keywords =
  [ "Armor"
  , "Attack"
  , "BladeRift"
  , "BlinkStrike"
  , "BossPlural"
  , "Cast"
  , "Charm"
  , "Chill"
  , "Cloud"
  , "Common"
  , "Crit"
  , "Curse"
  , "Dash"
  , "Deflect"
  , "Dodge"
  , "Encounter"
  , "EncounterAlt"
  , "EncounterPlural"
  , "ExtraChance"
  , "FishingPoint"
  , "Fountain"
  , "GodBoonPlural"
  , "Invulnerable"
  , "Mark"
  , "Poison"
  , "PomPlural"
  , "Rarity"
  , "Revenge"
  , "RoomAlt"
  , "Rupture"
  , "Special"
  , "Stagger"
  , "Static"
  , "Sturdy"
  , "Traps"
  , "Weak"
  , "Wrath"
  , "WrathDerivedStocks"
  , "WrathGauge"
  ]

type Tooltip
  = PercentTooltip String
  | Tooltip String
