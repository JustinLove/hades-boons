module DxfDecodeTest exposing (..)

import Dxf exposing (..)
import Dxf.Decode exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "DXF"
    [ describe "basic values"
      [ test "x" <| \_ ->
        x (X 0)
          |> Expect.equal (Ok 0)
      , test "y" <| \_ ->
        y (Y 0)
          |> Expect.equal (Ok 0)
      ]
    , describe "compound values"
      [ test "tag" <| \_ ->
        tag 10 x [(10, X 0)]
          |> Expect.equal (Ok 0)
      , test "succeed" <| \_ ->
        succeed 1 [(10, X 0)]
          |> Expect.equal (Ok 1)
      , test "map" <| \_ ->
        map (\x -> x + 1) (tag 10 x) [(10, X 0)]
          |> Expect.equal (Ok 1)
      , test "andThen" <| \_ ->
        andThen (\x -> succeed (x + 1)) (tag 10 x) [(10, X 0)]
          |> Expect.equal (Ok 1)
      , test "withTag" <| \_ ->
        (succeed (\x -> x + 1) |> withTag 10 x) [(10, X 0)]
          |> Expect.equal (Ok 1)
      , test "point" <| \_ ->
        point [(10, X 0), (20, Y 1)]
          |> Expect.equal (Ok (0,1))
      , test "id" <| \_ ->
        id [(1000, Text "boon:TestBoon")]
          |> Expect.equal (Ok "TestBoon")
      , test "maybe true" <| \_ ->
        (maybe id) [(1000, Text "boon:TestBoon")]
          |> Expect.equal (Ok (Just "TestBoon"))
      , test "maybe false" <| \_ ->
        (maybe id) [(2000, Text "boon:TestBoon")]
          |> Expect.equal (Ok Nothing)
      ]
    , describe "decoding entities"
      [ test "entity" <| \_ ->
        entity PointEntity point [(0, EntityType PointEntity), (10, X 0), (20, Y 0)]
          |> Expect.equal (Ok (0,0))
      , test "single entities" <| \_ ->
        entitiesStep PointEntity point [] [(0, EntityType PointEntity), (10, X 0), (20, Y 0)]
          |> Expect.equal (Ok [(0,0)])
      , test "consecutive entities" <| \_ ->
        entitiesStep PointEntity point []
          [ (0, EntityType PointEntity), (10, X 0), (20, Y 0)
          , (0, EntityType PointEntity), (10, X 1), (20, Y 1)
          ]
          |> Expect.equal (Ok [(0,0), (1,1)])
      , test "mixed entities" <| \_ ->
        entitiesStep PointEntity point []
          [ (0, EntityType PointEntity), (10, X 0), (20, Y 0)
          , (0, EntityType LineEntity), (10, X 2), (20, Y 2)
          , (0, EntityType PointEntity), (10, X 1), (20, Y 1)
          ]
          |> Expect.equal (Ok [(0,0), (1,1)])
      , test "advance" <| \_ ->
        entities PointEntity point
          [ (0, EntityType SectionStart), (2, Name "HEADER")
          , (0, EntityType PointEntity), (10, X 3), (20, Y 3)
          , (0, EntityType SectionStart), (2, Name "ENTITIES")
          , (0, EntityType PointEntity), (10, X 0), (20, Y 0)
          , (0, EntityType LineEntity), (10, X 2), (20, Y 2)
          , (0, EntityType PointEntity), (10, X 1), (20, Y 1)
          , (0, EntityType SectionEnd)
          , (0, EntityType PointEntity), (10, X 4), (20, Y 4)
          ]
          |> Expect.equal (Ok [(0,0), (1,1)])
      , test "polylines" <| \_ ->
        every 10 point
          [ (0, EntityType LWPolyLineEntity), (10, X 0), (20, Y 0), (10, X 10), (20, Y 20)
          , (0, EntityType PointEntity), (10, X 1), (20, Y 1)
          ]
          |> Expect.equal (Ok [(0,0), (10,20)])
      ]
    , describe "from string"
      [ test "decodeString" <| \_ ->
        decodeString boons entitySection
          |> Expect.equal (Ok [Boon "TestBoon" (20,20)])
      ]
    ]

type alias Boon =
  { id : String
  , point : (Float, Float)
  }

boons : Decoder (List Boon)
boons =
  entities PointEntity boon

boon : Decoder Boon
boon =
  succeed Boon
    |> with id
    |> with point

point : Decoder (Float, Float)
point =
  succeed Tuple.pair
    |> with (tag 10 x)
    |> with (tag 20 y)

id : Decoder String
id =
  andThen extractId (tag 1000 text)

extractId : String -> Decoder String
extractId =
  String.split(":")
    >> List.drop 1
    >> List.head
    >> Maybe.map succeed
    >> Maybe.withDefault (fail "point not tagged")

entitySection = """  0
SECTION
  2
ENTITIES
  0
POINT
  5
38
330
241
100
AcDbEntity
  8
0
100
AcDbPoint
 10
20.0
 20
20.0
 30
0.0
1001
QCAD
1000
boon:TestBoon
  0
LINE
  5
50
330
241
100
AcDbEntity
  8
BasicsCreated
100
AcDbLine
 10
27.0
 20
20.0
 30
0.0
 11
47.0
 21
40.0
 31
0.0
  0
ENDSEC
"""
