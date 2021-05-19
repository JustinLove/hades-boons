module Layout exposing
  ( Layout
  , GroupId
  , Placement
  , Connection
  , ArcType
  , ConnectionType(..)
  , getPlacement
  , calculateActiveGroups
  )

import Traits exposing (TraitId)

import Set exposing (Set)

type alias GroupId = String

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
  { group : GroupId
  , link : Maybe TraitId
  , shape : ConnectionType
  }

getPlacement : Layout -> TraitId -> Maybe (Float, Float)
getPlacement layout trait =
  layout.placements
    |> List.filter (\{id} -> id == trait)
    |> List.head
    |> Maybe.map .point

calculateActiveGroups : Layout -> Set TraitId -> Set GroupId
calculateActiveGroups layout activeTraits =
  let
    fromTraits = 
      layout.connections
        |> List.foldl
          (\{link, group} active ->
            case link of
              Just id ->
                if Set.member id activeTraits then
                  Set.insert group active
                else
                  active
              Nothing ->
                active
          )
          Set.empty
  in
    layout.connections
      |> List.foldl
        (\{link, group} active ->
          case link of
            Just id ->
              if Set.member id active then
                Set.insert group active
              else
                active
            Nothing ->
              active
        )
        fromTraits
    |> Debug.log "active groups"

