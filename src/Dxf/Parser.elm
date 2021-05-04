module Dxf.Parser exposing (..)

import Parser.Advanced exposing (..)

type alias Context = String
type alias Problem = String
type alias DxfParser a = Parser Context Problem a

type alias GroupCode = Int

type Value
  = UnknownCode GroupCode String
  | EntityType EntityType -- 0
  | PrimaryText String -- 1
  | Name String -- 2
  | Text String -- 3,4
  | EntityHandle String -- 5
  | LineType String -- 6
  | TextStyle String -- 7
  | Layer String -- 8
  | Variable String -- 9
  | X Float -- 10
  | Y Float -- 20
  | Z Float -- 30
  | FloatValue Float -- 40-48
  | RepeatedFloat Float -- 49
  | Angle Float -- 50-58
  | Visibility Int -- 60
  | ColorNumber Int -- 62
  | Integer Int -- 70-78 (16), 90-99 (32)
  | SubclassDataMarker String -- 100
  | ControlStringStart String -- 102
  | ControlStringEnd -- 102
  | Boolean Bool -- 290-299
  | SoftPointer String -- 330-339
  | HardPointer String -- 340-349
  | SoftOwnerHandle String -- 350-359
  | HardOwnerHandle String -- 360-369
  | LineWeight Int -- 370-379
  | PlotStyleName String -- 390-399
  | TrueColor Int -- 420-427

type EntityType
  = UnknownType String
  | Section

newline = Token "\n" "Expecting end of line"

groupCode : DxfParser GroupCode
groupCode =
  succeed identity
    |. spaces
    |= int "looking for a group code" "invalid group code"
    |. symbol newline

intValue : DxfParser Int
intValue =
  succeed identity
    |. spaces
    |= negatable (int "looking for an integer" "invalid integer")
    |. symbol newline

int16Value = intValue
int32Value = intValue

floatValue : DxfParser Float
floatValue =
  succeed identity
    |. spaces
    |= negatable (float "looking for an float" "invalid float")
    |. symbol newline

negatable : DxfParser number -> DxfParser number
negatable parser =
  oneOf
    [ succeed negate
      |. symbol (Token "-" "looking for a negative sign")
      |= parser
    , parser
    ]

stringValue : DxfParser String
stringValue =
  getChompedString (chompWhile (\c -> c /= '\n'))
    |. symbol newline

hexId : DxfParser String
hexId =
  getChompedString (chompWhile Char.isHexDigit)
    |. symbol newline

boolValue : DxfParser Bool
boolValue =
  succeed identity
    |. spaces
    |= bool
    |. symbol newline

bool : DxfParser Bool
bool =
  oneOf
    [ succeed True
      |. symbol (Token "1" "looking for a boolean 1")
    , succeed False
      |. symbol (Token "0" "looking for a boolean 0")
    ]

controlString : DxfParser Value
controlString =
  oneOf
    [ succeed ControlStringStart
      |. symbol (Token "{" "looking for an open {")
      |= stringValue
    , succeed ControlStringEnd
      |. symbol (Token "}" "looking for an close }")
      |. symbol newline
    ]

entityType : DxfParser EntityType
entityType =
  stringValue
    |> map entityMap

entityMap : String -> EntityType
entityMap s =
  case s of
    "SECTION" -> Section
    _ -> UnknownType s

value : DxfParser Value
value =
  groupCode
    |> andThen valueForCode

valueForCode : GroupCode -> DxfParser Value
valueForCode code =
  if code == 0 then
    map EntityType entityType
  else if code == 1 then
    map PrimaryText stringValue
  else if code == 2 then
    map Name stringValue
  else if code >= 3 && code <= 4 then
    map Text stringValue
  else if code == 5 then
    map EntityHandle hexId
  else if code == 6 then
    map LineType stringValue
  else if code == 7 then
    map TextStyle stringValue
  else if code == 8 then
    map Layer stringValue
  else if code == 9 then
    map Variable stringValue
  else if code >= 10 && code <= 18 then
    map X floatValue
  else if code >= 20 && code <= 28 then
    map Y floatValue
  else if code >= 30 && code <= 37 then
    map Z floatValue
  else if code >= 40 && code <= 48 then
    map FloatValue floatValue
  else if code == 49 then
    map RepeatedFloat floatValue
  else if code >= 50 && code <= 58 then
    map Angle floatValue
  else if code == 60 then
    map Visibility int16Value
  else if code == 61 then
    map Integer int16Value
  else if code == 62 then
    map ColorNumber int16Value
  else if code == 63 then
    map Integer int16Value
  else if code >= 70 && code <= 79 then
    map Integer int16Value
  else if code >= 90 && code <= 99 then
    map Integer int32Value
  else if code == 100 then
    map SubclassDataMarker stringValue
  else if code == 102 then
    controlString
  else if code >= 110 && code <= 112 then
    map X floatValue
  else if code >= 120 && code <= 122 then
    map Y floatValue
  else if code >= 130 && code <= 132 then
    map Z floatValue
  else if code >= 140 && code <= 149 then
    map FloatValue floatValue
  else if code >= 170 && code <= 179 then
    map Integer int16Value
  else if code >= 270 && code <= 279 then
    map Integer int16Value
  else if code >= 280 && code <= 289 then
    map Integer int16Value
  else if code >= 290 && code <= 299 then
    map Boolean boolValue
  else if code >= 300 && code <= 309 then
    map Text stringValue
  else if code >= 330 && code <= 339 then
    map SoftPointer hexId
  else if code >= 340 && code <= 349 then
    map HardPointer hexId
  else if code >= 350 && code <= 359 then
    map SoftOwnerHandle hexId
  else if code >= 360 && code <= 369 then
    map HardOwnerHandle hexId
  else if code >= 370 && code <= 379 then
    map LineWeight int16Value
  else if code >= 390 && code <= 399 then
    map PlotStyleName hexId
  else if code >= 420 && code <= 427 then
    map TrueColor int32Value
  else if code == 1000 then
    map Text stringValue
  else
    map (UnknownCode code) stringValue

