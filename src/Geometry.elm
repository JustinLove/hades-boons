module Geometry exposing (..)

type alias Point = (Float, Float)

sub : Point -> Point -> Point
sub (ax, ay) (bx, by) =
  (ax - bx, ay - by)

add : Point -> Point -> Point
add (ax, ay) (bx, by) =
  (ax + bx, ay + by)

scale : Float -> Point -> Point
scale s (x, y) =
  (x * s, y * s)

midpoint : Point -> Point -> Point
midpoint (xa,ya) (xb,yb) =
  (xa + (xb - xa)/2, ya + ((yb - ya)/2))

interpolate : Float -> Point -> Point -> Point
interpolate factor (xa,ya) (xb,yb) =
  (xa + (xb - xa)*factor, ya + ((yb - ya)*factor))

length : Point -> Float
length (x,y) =
  sqrt ((x^2) + (y^2))

normalize : Point -> Point
normalize (x,y) =
  let l = length (x,y) in
  (x / l, y / l)

perpendicular : Point -> Point
perpendicular (x,y) =
  (1, -x / y)

intersection : (Point, Point) -> (Point, Point) -> Point
intersection ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) =
  let
    dx12 = x1 - x2
    dy12 = y1 - y2
    dx34 = x3 - x4
    dy34 = y3 - y4
    denom = (dx12 * dy34) - (dy12 * dx34)
    t12 = x1 * y2 - y1 * x2
    t34 = x3 * y4 - y3 * x4
    nx = (t12 * dx34) - (dx12 * t34)
    ny = (t12 * dy34) - (dy12 * t34)
  in
    (nx / denom, ny / denom)
