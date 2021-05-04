module DxfParserTest exposing (..)

import Dxf.Parser exposing (..)

import Parser.Advanced exposing (run)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "DXF"
    [ describe "lines"
      [ test "group code" <| \_ ->
        run groupCode "  0\n"
          |> Expect.equal (Ok 0)
      , test "string value" <| \_ ->
        run stringValue "SECTION\n"
          |> Expect.equal (Ok "SECTION")
      ]
    , describe "tagged values"
      [ parses value "  0\nSECTION\n" (EntityType Section)
      , parses value "  2\nHEADER\n" (Name "HEADER")
      , parses value "  9\n$ACADVER\n" (Variable "$ACADVER")
      , parses value "  1\nAC1027\n" (PrimaryText "AC1027")
      , parses value " 70\n   105\n" (Integer 105)
      , parses value "  3\nANSI_1252\n" (Text "ANSI_1252")
      , parses value " 10\n0.0\n" (X 0.0)
      , parses value " 20\n0.0\n" (Y 0.0)
      , parses value " 30\n0.0\n" (Z 0.0)
      , parses value "  7\nStandard\n" (TextStyle "Standard")
      , parses value "  8\n0\n" (Layer "0")
      , parses value "  6\nByLayer\n" (LineType "ByLayer")
      , parses value " 62\n   256\n" (ColorNumber 256)
      , parses value " 40\n1.0\n" (FloatValue 1.0)
      , parses value " 40\n-1.0\n" (FloatValue -1.0)
      , parses value " 50\n0.0\n" (Angle 0.0)
      , parses value "  5\n313\n" (EntityHandle "313")
      , parses value "370\n    -1\n" (LineWeight -1)
      , parses value "280\n     0\n" (Integer 0)
      , parses value "290\n     0\n" (Boolean False)
      , parses value "347\n266\n" (HardPointer "266")
      , parses value " 90\n0\n" (Integer 0)
      , parses value "330\n0\n" (SoftPointer "0")
      , parses value "100\nAcDbSymbolTable\n" (SubclassDataMarker "AcDbSymbolTable")
      , parses value "110\n0.0\n" (X 0.0)
      , parses value "120\n0.0\n" (Y 0.0)
      , parses value "130\n0.0\n" (Z 0.0)
      , parses value "146\n0.0\n" (FloatValue 0.0)
      , parses value " 60\n3\n" (Visibility 3)
      , parses value " 61\n5\n" (Integer 5)
      , parses value "361\n28F\n" (HardOwnerHandle "28F")
      , parses value " 49\n63.5\n" (RepeatedFloat 63.5)
      , parses value "390\n232\n" (PlotStyleName "232")
      , parses value "420\n        0\n" (TrueColor 0)
      , parses value "350\n231\n" (SoftOwnerHandle "231")
      , parses value "102\n{ACAD_REACTORS\n" (ControlStringStart "ACAD_REACTORS")
      , parses value "102\n}\n" (ControlStringEnd)
      , parses value "300\n1:10\n" (Text "1:10")
      , parses value "170\n     2\n" (Integer 2)
      , parses value "274\n    -2\n" (Integer -2)
      , parses value "1000\n#ffffff\n" (Text "#ffffff")
      ]
    ]

parses : DxfParser a -> String -> a -> Test
parses parser input output =
  test input <| \_ ->
    run parser input
      |> Expect.equal (Ok output)
