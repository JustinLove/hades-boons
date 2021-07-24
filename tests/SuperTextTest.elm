module SuperTextTest exposing (..)

import SuperText exposing (..)
import SuperText.Parser exposing (..)

import Parser.Advanced exposing (run)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
  describe "SuperText"
    [ describe "Text"
      [ test "text" <| \_ ->
        run text "Your "
          |> Expect.equal (Ok "Your ")
      ]
    , describe "Keywords"
      [ test "WrathGauge" <| \_ ->
        run keyword "$Keywords.WrathGauge"
          |> Expect.equal (Ok (Keyword "WrathGauge"))
      , test "RoomAlt" <| \_ ->
        run keyword "$Keywords.RoomAlt"
          |> Expect.equal (Ok (Keyword "RoomAlt"))
      ]
    , describe "TooltipData"
      [ test "TooltipDuration" <| \_ ->
        run tooltipData "$TooltipData.TooltipDuration"
          |> Expect.equal (Ok (Tooltip "TooltipDuration"))
      , test "percent" <| \_ ->
        run tooltipData "$TooltipData.TooltipCritChance:P"
          |> Expect.equal (Ok (PercentTooltip "TooltipCritChance"))
      ]
    , describe "Icons"
      [ test "Ammo" <| \_ ->
        run icon "!Icons.Ammo"
          |> Expect.equal (Ok Ammo)
      ]
    , describe "Formatting"
      [ test "BoldFormatGraft" <| \_ ->
        run format "#BoldFormatGraft"
          |> Expect.equal (Ok BoldGraft)
      , test "PreviousFormat" <| \_ ->
        run format "#PreviousFormat"
          |> Expect.equal (Ok PreviousFormat)
      ]
    , describe "code"
      [ test "WrathGauge" <| \_ ->
        run code "{$Keywords.WrathGauge}"
          |> Expect.equal (Ok (Keywords (Keyword "WrathGauge")))
      , test "Ammo" <| \_ ->
        run code "{!Icons.Ammo}"
          |> Expect.equal (Ok (Icons Ammo))
      , test "TooltipDuration" <| \_ ->
        run code "{$TooltipData.TooltipDuration}"
          |> Expect.equal (Ok (TooltipData (Tooltip "TooltipDuration")))
      , test "percent" <| \_ ->
        run code "{$TooltipData.TooltipCritChance:P}"
          |> Expect.equal (Ok (TooltipData (PercentTooltip "TooltipCritChance")))
      , test "BoldFormatGraft" <| \_ ->
        run code "{#BoldFormatGraft}"
          |> Expect.equal (Ok (Format BoldGraft []))
      ]
    , describe "parts "
      [ test "simple text" <| \_ ->
        run part "Your charges up automatically."
          |> Expect.equal (Ok (Text "Your charges up automatically."))
      , test "keywords" <| \_ ->
        run part "{$Keywords.WrathGauge}"
          |> Expect.equal (Ok (Keywords (Keyword "WrathGauge")))
      ]
    , describe "multi part texts"
      [ test "simple text" <| \_ ->
        run superText "Your charges up automatically."
          |> Expect.equal (Ok [ Text "Your charges up automatically."])
      , test "Quick Favor" <| \_ ->
        run superText "Your {$Keywords.WrathGauge} charges up automatically."
          |> Expect.equal (Ok [ Text "Your ", Keywords (Keyword "WrathGauge"), Text " charges up automatically."])
      , test "Auto Reload" <| \_ ->
        run superText "You regenerate {!Icons.Ammo} faster."
          |> Expect.equal (Ok [ Text "You regenerate ", Icons Ammo, Text " faster."])
      , test "Second Wind" <| \_ ->
        run superText "move speed for {#BoldFormatGraft}{$TooltipData.TooltipDuration} Sec.{#PreviousFormat}"
          |> Expect.equal (Ok
            [ Text "move speed for "
            , Format BoldGraft
              [ TooltipData (Tooltip "TooltipDuration")
              , Text " Sec."
              ]
            ])
      ]
    ]

