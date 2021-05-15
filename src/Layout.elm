module Layout exposing (Layout, Placement, Connection, ArcType, ConnectionType(..), getPlacement)

import Traits exposing (TraitId)

type alias Point = (Float, Float)

type alias Layout =
  { placements : List Placement
  , connections : List Connection
  }

type alias Placement =
  { id : TraitId
  , point : Point
  }

type alias ArcType =
  { center : Point
  , radius : Float
  , fromAngle : Float
  , toAngle : Float
  }

type ConnectionType
  = Line Point Point
  | Arc ArcType

foo : ConnectionType
foo = Arc (ArcType (0,0) 0 0 0)

type alias Connection =
  { group : String
  , link : Maybe TraitId
  , shape : ConnectionType
  }

getPlacement : Layout -> TraitId -> Maybe (Float, Float)
getPlacement layout trait =
  layout.placements
    |> List.filter (\{id} -> id == trait)
    |> List.head
    |> Maybe.map .point
