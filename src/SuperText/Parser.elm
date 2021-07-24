module SuperText.Parser exposing (..)

import SuperText exposing (..)
import Parser.Advanced exposing (..)

type alias Context = String
type alias Problem = String
type alias SuperTextParser a = Parser Context Problem a

parse : SuperTextParser (List SuperText)
parse =
  superText
    |. end "expecting end of text"

superText : SuperTextParser (List SuperText)
superText =
  loop [] superTextItem

superTextItem : List SuperText -> SuperTextParser (Step (List SuperText) (List SuperText))
superTextItem reversedValues =
  oneOf
    [ part
      |> andThen (\p ->
        case p of
          Format PreviousFormat _ ->
            succeed (Done (List.reverse reversedValues))
          Format f _ ->
            map (Format f) superText
              |> map (\sub -> Loop (sub :: reversedValues))
          _ ->
            succeed (Loop (p :: reversedValues))
      )
    , succeed (Done (List.reverse reversedValues))
    ]

part : SuperTextParser SuperText
part =
  oneOf
    [ code
    , text
      |> map Text
    ]

text : SuperTextParser String
text =
  succeed ()
    |. (chompIf (\c -> c /= '{') "Looking for a plain text")
    |. (chompWhile (\c -> c /= '{'))
    |> getChompedString

code : SuperTextParser SuperText
code =
  succeed identity
    |. symbol (Token "{" "looking for an open {")
    |= codeWord
    |. symbol (Token "}" "looking for a close }")

codeWord : SuperTextParser SuperText
codeWord =
  oneOf
    [ format
      |> map (\f -> Format f [])
    , icon
      |> map Icons
    , keyword
      |> map Keywords
    , tooltipData
      |> map TooltipData
    ]

format : SuperTextParser Format
format =
  succeed identity
    |. symbol (Token "#" "looking for format start")
    |= getChompedString (chompWhile (\c -> c /= '}'))
    |> andThen stringToFormat

icon : SuperTextParser Icon
icon =
  succeed identity
    |. symbol (Token "!Icons." "looking for icon start")
    |= getChompedString (chompWhile (\c -> c /= '}'))
    |> andThen stringToIcon

keyword : SuperTextParser Keyword
keyword =
  succeed Keyword
    |. symbol (Token "$Keywords." "looking for keyword start")
    |= getChompedString (chompWhile (\c -> c /= '}'))

tooltipData : SuperTextParser Tooltip
tooltipData =
  succeed (\s tag -> tag s)
    |. symbol (Token "$TooltipData." "looking for tooltip data")
    |= getChompedString (chompWhile (\c -> c /= '}' && c /= ':'))
    |= oneOf
      [ succeed PercentTooltip
        |. symbol (Token ":P" "checking for percent modifier")
      , succeed Tooltip
      ]

stringToFormat : String -> SuperTextParser Format
stringToFormat s =
  case s of
    "AltPenaltyFormat" -> succeed AltPenalty
    "AltUpgradeFormat" -> succeed AltUpgrade
    "BoldFormat" -> succeed Bold
    "BoldFormatGraft" -> succeed BoldGraft
    "ItalicFormat" -> succeed Italic
    "PreviousFormat" -> succeed PreviousFormat
    _ -> problem ("unknown format " ++ s)

stringToIcon : String -> SuperTextParser Icon
stringToIcon s =
  case s of
    "Ammo" -> succeed Ammo
    "Currency_Small" -> succeed Currency
    "GemSmall" -> succeed Gem
    "GiftPointSmall" -> succeed GiftPoint
    "Health_Small" -> succeed Health
    "Health_Small_Tooltip" -> succeed Health
    "HealthRestore_Small_Tooltip" -> succeed HealthRestore
    "HealthUp_Small" -> succeed HealthUp
    "KEnemyHealth_Small" -> succeed KEnemyHealth
    "MetaPoint_Small" -> succeed MetaPoint
    _ -> problem ("unknown icon " ++ s)
