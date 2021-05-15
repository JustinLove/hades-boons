module Dxf exposing (..)

type alias GroupCode = Int

type Value
  = UnknownCode GroupCode String
  | EntityType EntityType -- 0
  | PrimaryText String -- 1
  | Name String -- 2
  | Text String -- 3,4, 1000
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
  | EntitiesFollow Int -- 67
  | Space Int -- 67
  | ViewportStatus Int -- 68
  | ViewportId Int -- 69
  | Integer Int -- 70-78 (16), 90-99 (32), 160 (64)
  | SubclassDataMarker String -- 100
  | ControlStringStart String -- 102
  | ControlStringEnd -- 102
  | Handle String -- 105
  | Boolean Bool -- 290-299
  | SoftPointer String -- 330-339
  | HardPointer String -- 340-349
  | SoftOwnerHandle String -- 350-359
  | HardOwnerHandle String -- 360-369
  | LineWeight Int -- 370-379
  | PlotStyleNameEnum Int -- 380-389
  | PlotStyleNameObject String -- 390-399
  | TrueColor Int -- 420-427
  | ApplicationName String -- 1001 (technically limited 31 bytes)

type EntityType
  = UnknownType String
  | ApplicationId
  | ArcEntity
  | BlockRecord
  | BlockStart
  | BlockEnd
  | CellStyleMap
  | Class
  | Dictionary
  | DictionaryWithDefault
  | DictionaryVar
  | DimensionStyle
  | Material
  | MLeaderStyle
  | MLineStyle
  | LayerTable
  | LayoutEntity
  | LineEntity
  | Linetype
  | Placeholder
  | Point
  | Scale
  | SectionStart
  | SectionEnd
  | Solid
  | Style
  | Sun
  | TableStart
  | TableEnd
  | TableStyle
  | TextEntity
  | Viewport
  | ViewportTable
  | VisualStyle
  | XRecord
  | EOF
