module Layout exposing (Layout, Placement, Connection, getPlacement)

import Traits exposing (TraitId)

type alias Layout =
  { placements : List Placement
  , connections : List Connection
  }

type alias Placement =
  { id : TraitId
  , point : (Float, Float)
  }

type alias Connection =
  { group : String
  , link : Maybe TraitId
  , a : (Float, Float)
  , b : (Float, Float)
  }

getPlacement : Layout -> TraitId -> Maybe (Float, Float)
getPlacement layout trait =
  layout.placements
    |> List.filter (\{id} -> id == trait)
    |> List.head
    |> Maybe.map .point
