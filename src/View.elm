module View exposing (Msg(..), document, view, chartSize)

import BoonChart
import Traits exposing (TraitId, God)

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
    int = (\x -> x * scale |> round)
    spx = (\x -> x * scale |> round |> px)
    primaryBoonBacking =
      el
        [ Background.image "GUI/HUD/PrimaryBoons/PrimaryBoonBacking_6.png"
        , width (spx 138)
        , height (spx 1188)
        , centerX
        ]
        none
    slotIcon = (\path desc ->
      Input.button
        [ width (spx 288)
        , height (spx 187)
        , behindContent <|
          el
            [ Background.image "GUI/Screens/BoonIconFrames/primary.png"
            , width (spx 201)
            , height (spx 206)
            , centerX
            , centerY
            ] none
        ]
        { onPress = Nothing
        , label =
          (image
            [ width (spx 148)
            , centerX
            , centerY
            ]
            { src = path
            , description = desc
            }
          )
        }
      )
  in
  column
    [ alignLeft
    , centerY
    , height (spx 1188)
    , width (spx 288)
    , behindContent primaryBoonBacking
    , spacing (int -1)
    , paddingEach
      { top = (int 24)
      , right = 0
      , bottom = 0
      , left = 0
      }
    ]
    [ slotIcon "GUI/HUD/PrimaryBoons/SlotIcon_Attack.png" "Attack Slot"
    , slotIcon "GUI/HUD/PrimaryBoons/SlotIcon_Secondary.png" "Special Slot"
    , slotIcon "GUI/HUD/PrimaryBoons/SlotIcon_Ranged.png" "Cast Slot"
    , slotIcon "GUI/HUD/PrimaryBoons/SlotIcon_Dash.png" "Dash Slot"
    , slotIcon "GUI/HUD/PrimaryBoons/SlotIcon_Wrath.png" "Call Slot"
    ]

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
