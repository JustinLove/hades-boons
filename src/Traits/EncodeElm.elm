module Traits.EncodeElm exposing (..)

import Layout.EncodeElm
import Traits exposing (..)

import Dict exposing (Dict)
import Elm.CodeGen exposing (..)
import Set exposing (Set)

traits : Traits -> List Declaration
traits tr =
  [ valDecl
    Nothing
    (Just (typeVar "Traits"))
    "traits"
    (apply
      [ (fun "loadPreprocessedGodsAndDuoBoons")
      , (list (List.map godData (allGods tr)))
      , (list (List.map trait (duoBoons tr)))
      ]
    )
  ]

godData : GodData -> Expression
godData data =
  (apply [(fun "godData"), godDataRecord data])

godDataRecord : GodData -> Expression
godDataRecord data =
  record
    [ ("god", dataGod data |> god)
    , ("traits", basicBoons data |> List.map trait |> list)
    , ("layout", dataLayout data |> Layout.EncodeElm.layout)
    ]

god : God -> Expression
god g =
  g |> godName |> val

trait : Trait -> Expression
trait t =
  record
    [ ("icon", t.icon |> string)
    , ("trait", t.trait |> string)
    , ("name", t.name |> string)
    , ("description", t.description |> string)
    , ("slot", t.slot |> maybe string)
    , ("requiredSlottedTrait", t.requiredSlottedTrait |> maybe string)
    , ("requiredMetaUpgradeSelected", t.requiredMetaUpgradeSelected |> maybe string)
    , ("requiredFalseTraits", t.requiredFalseTraits |> set)
    , ("requirements", t.requirements |> requirements)
    , ("boonType", t.boonType |> boonType)
    , ("frame", t.frame |> frame)
    ]

requirements : Requirements -> Expression
requirements r =
  case r of
    None -> val "None"
    OneOf ts -> construct "OneOf" [parens (set ts)]
    OneFromEachSet lts -> construct "OneFromEachSet" [list (List.map set lts)]

set : Set TraitId -> Expression
set s =
  apply [(fqFun ["Set"] "fromList"), list (List.map string (Set.toList s))]

boonType : BoonType -> Expression
boonType bt =
  case bt of
    BasicBoon g -> construct "BasicBoon" [god g]
    DuoBoon a b -> construct "DuoBoon" [god a, god b]
    Keepsake -> val "Keepsake"

frame : Frame -> Expression
frame fr =
  case fr of
    CommonFrame -> val "CommonFrame"
    DuoFrame -> val "DuoFrame"
    KeepsakeFrame -> val "KeepsakeFrame"
    LegendaryFrame -> val "LegendaryFrame"

maybe : (a -> Expression) -> Maybe a -> Expression
maybe encoder m =
  case m of
    Just x -> construct "Just" [encoder x]
    Nothing -> val "Nothing"

texts : Dict String String -> List Declaration
texts tx =
  [ valDecl
    Nothing
    (Just (typeVar "Dict String String"))
    "texts"
    (apply
      [ (fun "Dict.fromList")
      , (list (List.map pair (Dict.toList tx)))
      ]
    )
  ]

pair : (String, String) -> Expression
pair (key, value) =
  tuple [string key, string value]
