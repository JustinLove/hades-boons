module TransformTest exposing (..)

import HadesBoons

import Expect exposing (Expectation)
import Test exposing (..)

type alias Point = (Float, Float)

viewable : Point -> Float -> HadesBoons.Viewable {}
viewable offset zoom =
  { windowWidth = 400
  , windowHeight = 300
  , rotation = 0
  , offset = offset
  , zoom = zoom
  }

focused : HadesBoons.Viewable {}
focused =
  viewable (0,0) 1
    |> HadesBoons.focusView (0,0) 1 0

suite : Test
suite =
  describe "transforms"
    [ describe "screen to chart"
      [ test "origin" <| \_ ->
        HadesBoons.screenToChart (viewable (0,0) 1) (0,0)
          |> equalPoints (0,0)
      , test "focused origin" <| \_ ->
        HadesBoons.screenToChart focused (0,0)
          |> equalPoints (-0.4836065573770492,-0.19672131147540983)
      , test "screen center" <| \_ ->
        HadesBoons.screenToChart focused (200, 150)
          |> equalPoints (0.33606557377049184,0.4180327868852459)
      , test "lower right" <| \_ ->
        HadesBoons.screenToChart focused (400, 300)
          |> equalPoints (1.1557377049180328,1.0327868852459017)
      , test "lower left" <| \_ ->
        HadesBoons.screenToChart focused (0, 300)
          |> equalPoints (-0.4836065573770492,1.0327868852459017)
      ]
    , describe "focus view"
      [ test "default" <| \_ ->
        HadesBoons.focusView (0,0) 1 0 (viewable (0,0) 1)
          |> Expect.equal (viewable (118,48) 1)
      , test "focus" <| \_ ->
        HadesBoons.focusView (0,0.28) 0.45 0 (viewable (0,0) 1)
          |> Expect.equal (viewable (-31.111111111111143,50.71111111111112) 2.2222222222222223)
      ]
    ]


equalPoints : Point -> Point -> Expectation
equalPoints (ex, ey) (ax, ay) =
  let e = 0.0001 in
  if ex - e < ax && ax < ex + e && ey - e < ay && ay < ey + e then
    Expect.pass
  else
    Expect.equal (ex, ey) (ax, ay)
