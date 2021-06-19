module View exposing (Msg(..), document, view, chartSize)

import BoonChart
import Traits exposing (TraitId, God, SlotId, BoonStatus(..))

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
--import Html.Events exposing (on)
import Set exposing (Set)
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
  | SelectSlot SlotId
  | SelectPrimary TraitId SlotId
  | SelectKeepsake TraitId
  | SelectSoul TraitId
  | SelectWeapon TraitId
  | Reset

type Frame
  = Primary
  | Common
  | Keepsake
  | MetaUpgrade
  | Tray

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
        , inFront displayReset
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
    , label = displayGodIcon (\s -> s/2 |> round) iconPath name
    }

displayGodIcon : (Float -> Int) -> String -> String -> Element Msg
displayGodIcon scaled iconPath name =
  (image
    [ centerX
    , centerY
    , height (px (scaled 80))
    , Border.rounded (scaled 40)
    , Background.color (rgba 0 0 0 0.5)
    ]
    { src = iconPath
    , description = name
    }
  )

displaySlotSelect model =
  let 
    scale = (((toFloat model.windowHeight) - 60) / 1188.0)
      |> atMost 0.5
      |> atLeast 0.2
    scaled = (\x -> x * scale |> round)
  in
  row
    [ alignLeft
    , centerY
    , spacing (scaled -70)
    ]
    [ column
      [ height (px (scaled 1188))
      , width (px (scaled 200))
      , behindContent (primaryBoonBacking scaled)
      , spacing (scaled -4)
      , paddingEach
        { top = (scaled 30)
        , right = 0
        , bottom = 0
        , left = 1
        }
      ]
      [ slotIcon model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Attack.png") "Attack Slot" "Melee"
      , slotIcon model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Secondary.png") "Special Slot" "Secondary"
      , slotIcon model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Ranged.png") "Cast Slot" "Ranged"
      , slotIcon model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Dash.png") "Dash Slot" "Rush"
      , slotIcon model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Wrath.png") "Call Slot" "Shout"
      , slotIcon model scaled Nothing "Keepsake" "Keepsake"
      ]
    , column
      [ height (px (scaled 1188))
      , width (px (scaled 200))
      , spacing (scaled -4)
      , paddingEach
        { top = (scaled 120)
        , right = 0
        , bottom = 0
        , left = 0
        }
      , htmlAttribute <| Html.Attributes.class "boon-column"
      ]
      [ slotIcon model scaled (Just "GUI/Screens/WeaponEnchantmentIcons/sword_base_icon.png") "Weapon" "Weapon"
      , metaUpgradeIcon model scaled
      ]
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

slotIcon model scaled mpath desc slot =
  let
    inSlot =
      model.traits
        |> Traits.boonsForSlot slot
        |> List.filter (\{trait} -> Set.member trait model.activeTraits)
        |> List.head
  in
  el
    [ if model.currentPrimaryMenu == Just slot then
        case slot of
          "Keepsake" -> onRight (keepsakeMenu model scaled)
          "Weapon" -> onRight (weaponMenu model scaled)
          _ -> onRight (slotMenu model scaled slot)
      else
        padding 0
    , width (px (scaled 200))
    ]
    (case inSlot of
      Just boon ->
        (boonIconButton (SelectSlot slot) scaled (if slot == "Keepsake" then Keepsake else Common) (Just boon.icon) boon.name)
      Nothing ->
        (boonIconButton (SelectSlot slot) scaled (if slot == "Keepsake" then Keepsake else Primary) mpath desc)
    )

metaUpgradeIcon model scaled =
  let
    inSlot =
      Traits.miscBoons
        |> List.filter (Traits.isSlot "Soul")
        |> List.filter (\{trait} -> Just trait == model.metaUpgrade)
        |> List.head
  in
  el
    [ if model.currentPrimaryMenu == Just "Soul" then
        onRight (soulMenu model scaled)
      else
        padding 0
    , width (px (scaled 200))
    ]
    (case inSlot of
      Just boon ->
        (metaIconButton (SelectSlot "Soul") scaled MetaUpgrade (Just boon.icon) boon.name)
      Nothing ->
        (metaIconButton (SelectSlot "Soul") scaled Tray Nothing "Soul")
    )

slotMenu model scaled slot =
  row
    [ centerY
    ]
    (model.traits
      |> Traits.linkableGods
      |> List.concatMap (\data ->
        data
          |> Traits.basicBoons
          |> List.filter (Traits.isSlot slot)
          |> List.filter (\{trait} ->
            case Dict.get trait model.boonStatus of
              Just Active -> True
              Just Available -> True
              Just Excluded -> False
              Just Unavailable -> False
              Nothing -> False
            )
          |> List.map (boonSelectButton scaled slot (Traits.dataGod data))
      )
    )

keepsakeMenu model scaled =
  row
    [ centerY
    ]
    (model.traits
      |> Traits.boonsForSlot "Keepsake"
      |> List.map (\boon -> boonIconButton (SelectKeepsake boon.trait) scaled Keepsake (Just boon.icon) (boon.name))
    )

soulMenu model scaled =
  row
    [ centerY
    ]
    (Traits.miscBoons
      |> List.filter (Traits.isSlot "Soul")
      |> List.map (\boon -> metaIconButton (SelectSoul boon.trait) scaled MetaUpgrade (Just boon.icon) (boon.name))
    )

weaponMenu model scaled =
  row
    [ centerY
    ]
    (Traits.miscBoons
      |> List.filter (Traits.isSlot "Weapon")
      |> List.map (\boon -> boonIconButton (SelectWeapon boon.trait) scaled Common (Just boon.icon) (boon.name))
    )

boonSelectButton : (Float -> Int) -> SlotId -> God -> Traits.Trait -> Element Msg
boonSelectButton scaled slot god boon =
  el
    [ inFront
      (el [ alignBottom, alignRight ] (displayGodIcon scaled (god |> Traits.godIcon) (god |> Traits.godName)))
    ]
    (boonIconButton (SelectPrimary boon.trait slot) scaled Common (Just boon.icon) boon.name)

boonIconButton : msg -> (Float -> Int) -> Frame -> Maybe String -> String -> Element msg
boonIconButton msg scaled frame mpath desc =
  el
    [ width (px (scaled 201))
    , height (px (scaled 187))
    , padding (scaled -100)
    , behindContent (displayFrame scaled frame)
    , behindContent (boonIcon scaled mpath desc)
    ]
    (Input.button
      [ Border.width (scaled 80)
      , Border.rounded (scaled 80)
      , Border.color (rgba 1 0 0 0)
      , centerX
      , centerY
      ]
      { onPress = Just msg
      , label = none
      }
    )

metaIconButton : msg -> (Float -> Int) -> Frame -> Maybe String -> String -> Element msg
metaIconButton msg scaled frame mpath desc =
  el
    [ width (px (scaled 201))
    , height (px (scaled 207))
    , padding (scaled -100)
    , behindContent (displayFrame scaled frame)
    , behindContent (metaIcon scaled mpath desc)
    ]
    (Input.button
      [ Border.width (scaled 80)
      , Border.rounded (scaled 80)
      , Border.color (rgba 1 0 0 0)
      , centerX
      , centerY
      ]
      { onPress = Just msg
      , label = none
      }
    )

boonIcon : (Float -> Int) -> Maybe String -> String -> Element msg
boonIcon scaled mpath desc =
  case mpath of
    Just path ->
      image
        [ width (px (scaled 150))
        , centerX
        , centerY
        , moveDown ((scaled 4) |> toFloat)
        ]
        { src = path
        , description = desc
        }
    Nothing ->
      none

metaIcon : (Float -> Int) -> Maybe String -> String -> Element msg
metaIcon scaled mpath desc =
  case mpath of
    Just path ->
      image
        [ width (px (scaled 100))
        , centerX
        , centerY
        , moveDown ((scaled 4) |> toFloat)
        ]
        { src = path
        , description = desc
        }
    Nothing ->
      none

displayFrame : (Float -> Int) -> Frame -> Element msg
displayFrame scaled frame = 
  case frame of
    Primary ->
      el
        [ Background.image "GUI/Screens/BoonIconFrames/primary.png"
        , width (px (scaled 201))
        , height (px (scaled 198))
        , centerX
        , centerY
        ] none
    Common ->
      el
        [ Background.image "GUI/Screens/BoonIconFrames/common.png"
        , width (px (scaled 201))
        , height (px (scaled 203))
        , centerX
        , centerY
        ] none
    Keepsake ->
      el
        [ Background.image "GUI/HUD/Primaryboons/Keepsake_Backing.png"
        , width (px (scaled 173))
        , height (px (scaled 114))
        , moveDown (scaled 50 |> toFloat)
        , centerX
        , centerY
        ] none
    MetaUpgrade ->
      el
        [ Background.image "GUI/Screens/BoonIconFrames/mirror_of_darkness.png"
        , width (px (scaled 204))
        , height (px (scaled 209))
        , centerX
        , centerY
        ] none
    Tray ->
      el
        [ Background.image "GUI/Screens/BoonIconFrames/tray.png"
        , width (px (scaled 187))
        , height (px (scaled 186))
        , centerX
        , centerY
        ] none


displayReset : Element Msg
displayReset =
  Input.button
    [ alignBottom
    , alignRight
    ]
    { onPress = Just Reset
    , label =
      (image
        [ width (px 80)
        ]
        { src = "GUI/Screens/LevelUpRespec.png"
        , description = "Reset"
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

atMost : number -> number -> number
atMost = min

atLeast : number -> number -> number
atLeast = max
