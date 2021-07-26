module SuperText exposing (..)

type SuperText
  = Text String
  | Format Format (List SuperText)
  | Icons Icon
  | Keywords Keyword
  | TempTextData Tooltip
  | TooltipData Tooltip

type Format
  = AltPenalty
  | AltUpgrade
  | Bold
  | BoldGraft
  | Italic
  | RareFormat
  | PreviousFormat
  | StatFormat
  | TooltipUpgrade
  | Upgrade

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
  , "BeowulfAspect"
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
  , "Invisible"
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
  , "WrathHades"
  ]

type Tooltip
  = PercentTooltip String
  | Tooltip String
