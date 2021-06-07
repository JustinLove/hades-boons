module View exposing (Msg(..), document, view, chartSize)

import BoonChart
import Traits exposing (TraitId, God, SlotId)

import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
--import Html.Events exposing (on)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode

type alias Point = (Float, Float)

type Msg
  = OnMouseMove Point
  | OnMouseDown Point
  | OnMouseUp Point
  | OnWheel Point Int
  | SelectGod God
  | SelectPrimary SlotId God
  | ViewAll

chartSize = 4069

document tagger model =
  { title = "Hades Boons"
  , body = [Html.map tagger (view model)]
  }

view model = 
  layout
    [ htmlAttribute <| Html.Attributes.class "dark"
    , htmlAttribute <| Html.Attributes.id "top"
    ] <|
    column
      [ height fill
        , width fill
        , clip
        , inFront displayFooter
        --, inFront (model.zoom |> String.fromFloat |> text)
        , inFront (displayGodSelect model)
        , inFront (displaySlotSelect model)
      ]
      [ BoonChart.boonChart
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.id "graph"
        ]
        { metrics = model.chartMetrics
        , activeBasicGroups = model.activeBasicGroups
        , activeDuoGroups = model.activeDuoGroups
        , boonStatus = model.boonStatus
        , onMouseMove = OnMouseMove
        , onMouseDown = OnMouseDown
        , onMouseUp = OnMouseUp
        , onWheel = OnWheel
        , drag = model.drag
        , offset = model.offset
        , zoom = model.zoom
        , size = chartSize
        } |> html
      ]

displayGodSelect model =
  row [ spacing 10, centerX, padding 8 ]
    ( List.append
      (model.traits
        |> Traits.allGods
        |> List.map Traits.dataGod
        |> List.map displayGod
      )
      [ displayGodButton ViewAll "GUI/Screens/BoonIcons/godmode.png" "All"]
    )

displayGod : God -> Element Msg
displayGod god =
  displayGodButton (SelectGod god) (god |> Traits.godIcon) (god |> Traits.godName)

displayGodButton : Msg -> String -> String -> Element Msg
displayGodButton message iconPath name =
  Input.button
    [ width (px 40)
    ]
    { onPress = Just message
    , label =
      (image
        [ centerX
        , centerY
        , height (px 40)
        ]
        { src = iconPath
        , description = name
        }
      )
    }

displaySlotSelect model =
  let 
    scale = ((toFloat model.windowHeight) - 60) / 1188.0
    scaled = (\x -> x * scale |> round)
  in
  column
    [ alignLeft
    , centerY
    , height (px (scaled 1188))
    , width (px (scaled 288))
    , behindContent (primaryBoonBacking scaled)
    , spacing (scaled -1)
    , paddingEach
      { top = (scaled 24)
      , right = 0
      , bottom = 0
      , left = 1
      }
    ]
    [ slotIcon model scaled "GUI/HUD/PrimaryBoons/SlotIcon_Attack.png" "Attack Slot" "Melee"
    , slotIcon model scaled "GUI/HUD/PrimaryBoons/SlotIcon_Secondary.png" "Special Slot" "Secondary"
    , slotIcon model scaled "GUI/HUD/PrimaryBoons/SlotIcon_Ranged.png" "Cast Slot" "Ranged"
    , slotIcon model scaled "GUI/HUD/PrimaryBoons/SlotIcon_Dash.png" "Dash Slot" "Rush"
    , slotIcon model scaled "GUI/HUD/PrimaryBoons/SlotIcon_Wrath.png" "Call Slot" "Shout"
    ]

primaryBoonBacking : (Float -> Int) -> Element Msg
primaryBoonBacking scaled =
  el
    [ Background.image "GUI/HUD/PrimaryBoons/PrimaryBoonBacking_6.png"
    , width (px (scaled 138))
    , height (px (scaled 1188))
    , centerX
    ]
    none


slotIcon model scaled path desc slot =
  el
    [ inFront (slotMenu model scaled slot)
    , width (px (scaled 288))
    ]
    (boonIcon scaled path desc)

slotMenu model scaled slot =
  row
    [ paddingEach
      { top = 0
      , right = 0
      , bottom = 0
      , left = (scaled 200)
      }
    , centerY
    ]
    (model.traits
      |> Traits.linkableGods
      |> List.map Traits.dataGod
      |> List.map (boonSelectForGod slot)
    )

boonSelectForGod : SlotId -> God -> Element Msg
boonSelectForGod slot god =
  displayGodButton (SelectPrimary slot god) (god |> Traits.godIcon) (god |> Traits.godName)

boonIcon scaled path desc =
  Input.button
    [ width (px (scaled 201))
    , height (px (scaled 187))
    , centerX
    , behindContent <|
      el
        [ Background.image "GUI/Screens/BoonIconFrames/primary.png"
        , width (px (scaled 201))
        , height (px (scaled 206))
        , centerX
        , centerY
        ] none
    ]
    { onPress = Nothing
    , label =
      (image
        [ width (px (scaled 148))
        , centerX
        , centerY
        ]
        { src = path
        , description = desc
        }
      )
    }

displayFooter : Element msg
displayFooter =
  row [ Region.footer, spacing 10, alignBottom, padding 8 ]
    {-[ link []
      { url = "https://github.com/JustinLove/"
      , label = row [] [ icon "github", text "hades-boons" ]
      }
      -}
    [ link []
      { url = "https://twitter.com/wondible"
      , label = row [] [ icon "twitter", text "@wondible" ]
      }
    , link []
      { url = "https://twitch.tv/wondible"
      , label = row [] [ icon "twitch", text "wondible" ]
      }
    ]

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html
