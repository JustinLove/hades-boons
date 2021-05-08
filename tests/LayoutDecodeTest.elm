module LayoutDecodeTest exposing (..)

import Layout exposing (..)
import Layout.DecodeXml exposing (..)

import Xml.Decode exposing (run)
import XmlParser exposing (Node(..))

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
  describe "Diagrams.net"
    [ test "point" <| \_ ->

      run point """<mxGeometry x="40" y="530" width="80" height="80" as="geometry"/>"""
        |> Expect.equal (Ok (40, 530))
    , test "placement" <| \_ ->

      run placement """
        <mxCell id="6N4ug1b89jXwTqVvpMMG-1" value="ShieldLoadAmmo_DemeterRangedTrait" style="rhombus;whiteSpace=wrap;html=1;" vertex="1" parent="1">
          <mxGeometry x="40" y="530" width="80" height="80" as="geometry"/>
        </mxCell>
"""
        |> Expect.equal (Ok {id = "ShieldLoadAmmo_DemeterRangedTrait", point = (40, 530)})
    , test "layout" <| \_ ->

      run layout """
<?xml version="1.0" encoding="UTF-8"?>
<mxfile host="app.diagrams.net" modified="2021-05-01T14:29:01.015Z" agent="5.0 (Windows)" etag="86Me5WSnLPLGcz1nTOXn" version="14.6.9">
  <diagram id="dRneyWYuNGLPaZCr_-yl" name="Page-1">
    <mxGraphModel dx="1311" dy="574" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="850" pageHeight="1100" math="0" shadow="0">
      <root>
        <mxCell id="0"/>
        <mxCell id="1" parent="0"/>
        <mxCell id="6N4ug1b89jXwTqVvpMMG-1" value="ShieldLoadAmmo_DemeterRangedTrait" style="rhombus;whiteSpace=wrap;html=1;" vertex="1" parent="1">
          <mxGeometry x="40" y="530" width="80" height="80" as="geometry"/>
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
"""
        |> Expect.equal (Ok [{id = "ShieldLoadAmmo_DemeterRangedTrait", point = (40, 530)}])
        {-
    , test "hacks" <| \_ ->

      run (Xml.Decode.path ["diagram", "mxGraphModel", "root", "mxCell"] (Xml.Decode.list Xml.Decode.node)) """
<?xml version="1.0" encoding="UTF-8"?>
<mxfile host="app.diagrams.net" modified="2021-05-01T14:29:01.015Z" agent="5.0 (Windows)" etag="86Me5WSnLPLGcz1nTOXn" version="14.6.9">
  <diagram id="dRneyWYuNGLPaZCr_-yl" name="Page-1">
    <mxGraphModel dx="1311" dy="574" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="850" pageHeight="1100" math="0" shadow="0">
      <root>
        <mxCell id="6N4ug1b89jXwTqVvpMMG-1" value="ShieldLoadAmmo_DemeterRangedTrait" style="rhombus;whiteSpace=wrap;html=1;" vertex="1" parent="1">
          <mxGeometry x="40" y="530" width="80" height="80" as="geometry"/>
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
"""
        |> Expect.equal (Ok [Element "foo" [] []])
        -}
    ]
