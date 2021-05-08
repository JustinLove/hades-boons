module DxfParserTest exposing (..)

import Dxf.Parser exposing (..)

import Parser.Advanced exposing (run)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "DXF"
    [ describe "lines"
      [ test "group code with n" <| \_ ->
        run groupCode "  0\n"
          |> Expect.equal (Ok 0)
      , test "group code with rn" <| \_ ->
        run groupCode "  0\r\n"
          |> Expect.equal (Ok 0)
      , test "string value" <| \_ ->
        run stringValue "SECTION\n"
          |> Expect.equal (Ok "SECTION")
      , test "string value rn" <| \_ ->
        run stringValue "SECTION\r\n"
          |> Expect.equal (Ok "SECTION")
      ]
    , describe "tagged values"
      [ parses value "  0\nSECTION\n" (EntityType SectionStart)
      , parses value "  0\r\nSECTION\r\n" (EntityType SectionStart)
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
      , parses value "390\n232\n" (PlotStyleNameObject "232")
      , parses value "420\n        0\n" (TrueColor 0)
      , parses value "350\n231\n" (SoftOwnerHandle "231")
      , parses value "102\n{ACAD_REACTORS\n" (ControlStringStart "ACAD_REACTORS")
      , parses value "102\n}\n" (ControlStringEnd)
      , parses value "300\n1:10\n" (Text "1:10")
      , parses value "170\n     2\n" (Integer 2)
      , parses value "274\n    -2\n" (Integer -2)
      , parses value "1000\n#ffffff\n" (Text "#ffffff")
      , parses value "380\n     0\n" (PlotStyleNameEnum 0)
      , parses value " 67\n     1\n" (Space 1)
      , parses value " 68\n     1\n" (ViewportStatus 1)
      , parses value " 69\n     1\n" (ViewportId 1)
      , parses value " 66\n     0\n" (EntitiesFollow 0)
      , parses value "160\n                 0\n" (Integer 0)
      , parses value "105\n2C9\n" (Handle "2C9")
      , parses value "1001\nQCAD\n" (ApplicationName "QCAD")
      , parses value "  0\nCLASS\n" (EntityType Class)
      , parses value "  0\nTABLE\n" (EntityType TableStart)
      , parses value "  0\nENDTAB\n" (EntityType TableEnd)
      , parses value "  0\nVPORT\n" (EntityType ViewportTable)
      , parses value "  0\nLTYPE\n" (EntityType Linetype)
      , parses value "  0\nSTYLE\n" (EntityType Style)
      , parses value "  0\nAPPID\n" (EntityType ApplicationId)
      , parses value "  0\nDIMSTYLE\n" (EntityType DimensionStyle)
      , parses value "  0\nBLOCK_RECORD\n" (EntityType BlockRecord)
      , parses value "  0\nBLOCK\n" (EntityType BlockStart)
      , parses value "  0\nENDBLK\n" (EntityType BlockEnd)
      , parses value "  0\nSOLID\n" (EntityType Solid)
      , parses value "  0\nLINE\n" (EntityType Line)
      , parses value "  0\nPOINT\n" (EntityType Point)
      , parses value "  0\nVIEWPORT\n" (EntityType Viewport)
      , parses value "  0\nSUN\n" (EntityType Sun)
      , parses value "  0\nDICTIONARY\n" (EntityType Dictionary)
      , parses value "  0\nACDBDICTIONARYWDFLT\n" (EntityType DictionaryWithDefault)
      , parses value "  0\nXRECORD\n" (EntityType XRecord)
      , parses value "  0\nLAYOUT\n" (EntityType Layout)
      , parses value "  0\nMATERIAL\n" (EntityType Material)
      , parses value "  0\nMLEADERSTYLE\n" (EntityType MLeaderStyle)
      , parses value "  0\nMLINESTYLE\n" (EntityType MLineStyle)
      , parses value "  0\nACDBPLACEHOLDER\n" (EntityType Placeholder)
      , parses value "  0\nSCALE\n" (EntityType Scale)
      , parses value "  0\nTABLESTYLE\n" (EntityType TableStyle)
      , parses value "  0\nVISUALSTYLE\n" (EntityType VisualStyle)
      , parses value "  0\nDICTIONARYVAR\n" (EntityType DictionaryVar)
      , parses value "  0\nCELLSTYLEMAP\n" (EntityType CellStyleMap)
      , parses value "  0\nLAYER\n" (EntityType LayerTable)
      ]
    , describe "stream"
      [ test "empty string" <| \_ ->
        run (valuesUntil (EntityType SectionEnd)) "" 
          |> Expect.equal (Ok [])
      , test "single item" <| \_ ->
        run (valuesUntil (EntityType SectionEnd)) "  0\nSECTION\n"
          |> Expect.equal (Ok [(0, EntityType SectionStart)])
      , test "small header" <| \_ ->
        run (valuesUntil (EntityType SectionEnd)) smallHeader
          |> Expect.equal (Ok smallHeaderValues)
      , test "section and tail" <| \_ ->
        run (valuesUntil (EntityType SectionEnd)) (smallHeader ++ smallHeader)
          |> Expect.equal (Ok smallHeaderValues)
      ]
    , describe "value parsers"
      [ parses (entity "SECTION") "  0\nSECTION\n" ()
      , parses name "  2\nHEADER\n" "HEADER"
      ]
    , describe "structures"
      [ test "empty section" <| \_ ->
        run section "  2\nHEADER\n  0\nENDSEC\n" 
          |> Expect.equal (Ok (Section "HEADER" []))
      , test "small section" <| \_ ->
        run section smallHeader
          |> Expect.equal (Ok smallSection)
      , test "small file" <| \_ ->
        run dxf (sectionText ++ smallHeader ++ sectionText ++ smallHeader ++ "  0\nEOF\n")
          |> Expect.equal (Ok
            [ smallSection
            , smallSection
            ])
      ]
    ]

parses : DxfParser a -> String -> a -> Test
parses parser input output =
  test input <| \_ ->
    run parser input
      |> Expect.equal (Ok output)

sectionText = "  0\nSECTION\n"

smallHeader = """  2
HEADER
  9
$ACADVER
  1
AC1027
  0
ENDSEC
"""

smallHeaderValues =
  [ (2, Name "HEADER")
  , (9, Variable "$ACADVER")
  , (1, PrimaryText "AC1027")
  ]

smallSection =
  (Section "HEADER" [(9, Variable "$ACADVER"), (1, PrimaryText "AC1027")])
