module Traits.EncodeElm exposing (..)

import Color
import Layout.EncodeElm
import Traits exposing (..)

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
    , ("lootColor", dataLootColor data |> color)
    , ("color", dataColor data |> color)
    , ("traits", basicBoons data |> List.map trait |> list)
    , ("layout", dataLayout data |> Layout.EncodeElm.layout)
    ]

god : God -> Expression
god g =
  g |> godName |> val

color : Color.Color -> Expression
color c = 
  (apply [(fun "Color.fromRgba"), colorRecord c])

colorRecord : Color.Color -> Expression
colorRecord c =
  let rgba = Color.toRgba c in
  record
    [ ("red", rgba.red |> float)
    , ("green", rgba.green |> float)
    , ("blue", rgba.blue |> float)
    , ("alpha", rgba.alpha |> float)
    ]

trait : Trait -> Expression
trait t =
  record
    [ ("icon", t.icon |> string)
    , ("trait", t.trait |> string)
    , ("name", t.name |> string)
    , ("slot", t.slot |> maybe string)
    , ("requiredSlottedTrait", t.requiredSlottedTrait |> maybe string)
    , ("requirements", t.requirements |> requirements)
    , ("boonType", t.boonType |> boonType)
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

maybe : (a -> Expression) -> Maybe a -> Expression
maybe encoder m =
  case m of
    Just x -> construct "Just" [encoder x]
    Nothing -> val "Nothing"

