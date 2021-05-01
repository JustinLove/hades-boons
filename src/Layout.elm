module Layout exposing (Layout, Placement, getPlacement)

import Traits exposing (TraitId)

type alias Layout = List Placement

type alias Placement =
  { id : TraitId
  , point : (Float, Float)
  }

getPlacement : Layout -> TraitId -> Maybe (Float, Float)
getPlacement layout trait =
  layout
    |> List.filter (\{id} -> id == trait)
    |> List.head
    |> Maybe.map .point
