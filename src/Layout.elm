module Layout exposing
  ( Layout
  , GroupId
  , Placement
  , Connection
  , ArcType
  , EllipticArcType
  , ConnectionType(..)
  , Winding(..)
  , Boundary(..)
  , empty
  , isEmpty
  , getPlacement
  , calculateActiveGroups
  )

import Set exposing (Set)

type alias TraitId = String
type alias GroupId = String

type alias Point = (Float, Float)

type alias Layout =
  { placements : List Placement
  , connections : List Connection
  , radius : Float
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
  , winding : Winding
  }

type alias EllipticArcType =
  { center : Point
  , majorAxis : Point
  , minorRatio : Float
  , fromAngle : Float
  , toAngle : Float
  }

type Winding
  = Clockwise
  | Counterclockwise

type Boundary
  = BoundaryArc ArcType
  | BoundaryLine Point Point

type ConnectionType
  = Arc ArcType
  | Area (List Boundary)
  | Circle Point Float
  | Dot Point
  | EllipticArc EllipticArcType
  | Line Point Point
  | PolyLine (List Point)

type alias Connection =
  { group : GroupId
  , link : List TraitId
  , shape : ConnectionType
  }

empty : Layout
empty = 
  { placements = []
  , connections = []
  , radius = 140
  }

isEmpty : Layout -> Bool
isEmpty {placements, connections} =
  (List.isEmpty placements) && (List.isEmpty connections)

getPlacement : Layout -> TraitId -> Maybe (Float, Float)
getPlacement layout trait =
  layout.placements
    |> List.filter (\{id} -> id == trait)
    |> List.head
    |> Maybe.map .point

calculateActiveGroups : Set TraitId -> Layout -> Set GroupId
calculateActiveGroups activeTraits layout =
  let
    foldActive {link, group} active =
      case link of
        [] ->
          active
        _ ->
          if List.all (\id -> Set.member id active || Set.member id activeTraits) link then
            Set.insert group active
          else
            active
    fromTraits = 
      layout.connections
        |> List.foldl foldActive Set.empty
  in
    layout.connections
      |> List.foldl foldActive fromTraits

