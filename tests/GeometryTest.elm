module GeometryTest exposing (..)

import Geometry exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "geometry"
    [ describe "dot product"
      [ test "right angle" <| \_ ->
        dotProduct (0,1) (1,0)
          |> Expect.equal 0
      , test "self" <| \_ ->
        dotProduct (0,1) (0,1)
          |> Expect.equal 1
      , test "communative" <| \_ ->
        dotProduct (1,2) (3,4)
          |> Expect.equal (dotProduct (3,4) (1,2))
      ]
    , describe "angle between"
      [ test "right angle" <| \_ ->
        angleBetween (0,1) (1,0)
          |> close (pi/2)
      , test "eighth angle" <| \_ ->
        angleBetween (1,0) (1,1)
          |> close (pi/4)
      , test "wider angle" <| \_ ->
        angleBetween (1,0) (-1,1)
          |> close (pi*3/4)
      , test "obtuse angle" <| \_ ->
        angleBetween (1,0) (0,-1)
          |> notClose (pi*6/4)
      ]
    ]

close = Expect.within (Expect.Absolute 0.0000001)
notClose = Expect.notWithin (Expect.Absolute 0.0000001)
