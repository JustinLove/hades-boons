module View exposing (Msg(..), document, view, chartDiameter, chartCenter)

import BoonChart
--import BoonChart.Svg
import BoonChart.Canvas
import Geometry
import Traits exposing (TraitId, God, SlotId, BoonStatus(..))

import Dict
import Canvas.Texture as Canvas
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
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
  | TextureLoaded String (Maybe Canvas.Texture)
  | SelectGod God
  | ViewAll
  | SelectSlot SlotId
  | SelectPrimary TraitId SlotId
  | SelectKeepsake TraitId
  | SelectSoul TraitId
  | SelectWeapon TraitId
  | Reset
  | Supergiant Bool

type Frame
  = Primary
  | Common
  | Keepsake
  | MetaUpgrade
  | Tray

chartDiameter : Int -> Int -> Float
chartDiameter width height =
  let
    displayWidth = ((width//1)-80-16)
    displayHeight = (height-40-16)
  in
    min displayWidth displayHeight
      |> toFloat

chartCenter : Int -> Int -> Point
chartCenter width height =
  ( (toFloat (width//1)+80)/2
  , (toFloat height+40)/2
  )

document tagger model =
  { title = "Hades Boon Chart"
  , body = [Html.map tagger (view model)]
  }

view model = 
  layout
    [ htmlAttribute <| Html.Attributes.class "dark"
    , htmlAttribute <| Html.Attributes.id "top"
    ] <|
    row
      [ height fill
      , width fill
      , clip
      , inFront (displayFooter model.artAttribution)
      , inFront (displayGodSelect model)
      , inFront (displaySlotSelect model)
      , inFront (displayDescription model)
      , inFront displayReset
      --, inFront (model.zoom |> printFloat |> text)
      --, inFront (model.rotation |> printFloat |> text)
      {-, inFront (displayWindowPoints
        [ model.offset
        , chartCenter model.windowWidth model.windowHeight
        ])
        -}
      ]
      [ {-BoonChart.Svg.boonChart
        [ Html.Attributes.style "width" "50vw"
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
        , diameter = chartDiameter model.windowWidth model.windowHeight
        } |> html
      , -} if model.windowKnown then
        BoonChart.Canvas.boonChart
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
          , onTexture = TextureLoaded
          , drag = model.drag
          , offset = model.offset
          , zoom = model.zoom
          , diameter = chartDiameter model.windowWidth model.windowHeight
          , width = model.windowWidth//1
          , height = model.windowHeight
          , textures = model.canvasTextures
          } |> html
        else
          none
      ]

displayGodSelect model =
  row [ spacing 10, centerX, padding 8 ]
    ( List.append
      (model.traits
        |> Traits.allGods
        |> List.map Traits.dataGod
        |> List.map displayGod
      )
      [ displayGodButton ViewAll "favicon.ico" "All"]
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
      [ slotTrayButton model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Attack.png") "Attack Slot" "Melee"
      , slotTrayButton model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Secondary.png") "Special Slot" "Secondary"
      , slotTrayButton model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Ranged.png") "Cast Slot" "Ranged"
      , slotTrayButton model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Dash.png") "Dash Slot" "Rush"
      , slotTrayButton model scaled (Just "GUI/HUD/PrimaryBoons/SlotIcon_Wrath.png") "Call Slot" "Shout"
      , keepsakeTrayButton model scaled
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
      [ slotTrayButton model scaled (Just "GUI/Screens/WeaponEnchantmentIcons/sword_base_icon.png") "Weapon" "Weapon"
      , metaTrayButton model scaled
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

slotTrayButton model scaled mpath desc slot =
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

keepsakeTrayButton model scaled =
  let
    inSlot =
      model.traits
        |> Traits.boonsForSlot "Keepsake"
        |> List.filter (\{trait} -> Set.member trait model.activeTraits)
        |> List.head
  in
  el
    [ if model.currentPrimaryMenu == Just "Keepsake" then
        onRight (keepsakeMenu model scaled)
      else
        padding 0
    , width (px (scaled 200))
    ]
    (case inSlot of
      Just boon ->
        (keepsakeIconButton (SelectSlot "Keepsake") scaled (Just boon.icon) boon.name Active)
      Nothing ->
        (keepsakeIconButton (SelectSlot "Keepsake") scaled Nothing "Keepsake" Active)
    )

metaTrayButton model scaled =
  let
    inSlot =
      Traits.miscBoons
        |> List.filter (Traits.isSlot "Soul")
        |> List.filter (\{trait} -> trait == model.metaUpgrade)
        |> List.head
  in
  el
    [ width (px (scaled 200))
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
      |> List.map (\boon -> keepsakeIconButton (SelectKeepsake boon.trait) scaled (Just boon.icon) (boon.name) ((if Set.intersect boon.requiredFalseTraits model.activeTraits |> Set.isEmpty then Available else Excluded)))
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

keepsakeIconButton : msg -> (Float -> Int) -> Maybe String -> String -> BoonStatus -> Element msg
keepsakeIconButton msg scaled mpath desc status =
  let
    keepsakeScale = \f -> scaled (f * 0.8)
  in
  el
    [ width (px (scaled 201))
    , height (px (scaled 187))
    , padding (scaled -100)
    , behindContent (displayFrame scaled Keepsake)
    , behindContent
      (el
        [ width fill
        , height fill
        , if status == Excluded then alpha 0.5 else alpha 1
        ]
        (boonIcon keepsakeScale mpath desc)
      )
    , if status == Excluded then
        inFront (lockedIcon keepsakeScale)
      else
        spacing 0
    ]
    (if status == Excluded then
      none
    else
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

lockedIcon : (Float -> Int) -> Element msg
lockedIcon scaled =
  image
    [ width (px (scaled 120))
    , centerX
    , centerY
    , moveDown ((scaled 4) |> toFloat)
    ]
    { src = "GUI/LockIcon/LockIcon0001.png"
    , description = "Locked"
    }

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
        [ Background.image "GUI/HUD/PrimaryBoons/Keepsake_Backing.png"
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
        , moveUp (scaled 4 |> toFloat)
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

displayDescription model =
  case model.descriptionBoon of
    Just id ->
      let
        desc =
          Traits.findBoon model.traits id
            |> Maybe.map .description
            |> Maybe.map (\d -> if d == "" then id else d)
            |> Maybe.withDefault id
      in
      el
        [ paddingEach
          { top = 0
          , right = resetSize * 5
          , bottom = footerSize * 2
          , left = 200
          }
        , alignBottom
        , centerX
        ]
        ( paragraph
          [ Background.color windowBackColor
          , Border.color buttonBorderColor
          , Border.width 2
          , Border.rounded 2
          , padding 10
          ]
          [ text desc ]
        )
    Nothing ->
      none

displayReset : Element Msg
displayReset =
  Input.button
    [ alignBottom
    , alignRight
    ]
    { onPress = Just Reset
    , label =
        row
          [ Font.size resetSize
          , centerY
          , padding 8
          , Background.color buttonBackColor
          , Border.color buttonBorderColor
          , Border.width 2
          , Border.rounded 2
          , moveUp 2
          ]
          [ icon "spinner11", text " Reset" ]
    }


displayFooter : Bool -> Element Msg
displayFooter artAttribution =
  row [ Region.footer, spacing 10, alignBottom, padding 8, Font.size footerSize, width fill ]
    [ link []
      { url = "https://github.com/JustinLove/hades-boons"
      , label = row [] [ icon "github", text "hades-boons" ]
      }
    , link []
      { url = "https://twitter.com/wondible"
      , label = row [] [ icon "twitter", text "@wondible" ]
      }
    , link []
      { url = "https://twitch.tv/wondible"
      , label = row [] [ icon "twitch", text "wondible" ]
      }
    , row
      [ Events.onMouseEnter (Supergiant True)
      , Events.onMouseLeave (Supergiant False)
      , above (if artAttribution then supergiantAttribution else none)
      , width fill
      ]
      [ text "Art by "
      , link
        [ Events.onFocus (Supergiant True)
        , Events.onLoseFocus (Supergiant False)
        ]
        { url = "https://www.supergiantgames.com/"
        , label = row [ Font.color (rgb 202 0 0) ] [ icon "star-full", text "Supergiant Games" ]
        }
      ]
    ]

supergiantAttribution : Element msg
supergiantAttribution =
  column [ padding (sizeStep -1 |> round) ]
    [ paragraph []
      [ text "God and boon icons and frames are from "
      , link []
        { url = "https://www.supergiantgames.com/games/hades"
        , label = text "Supergiant Games Hades"
        }
      , text "."
      ]
    , paragraph []
      [ text "This site is not developed or approved by Supergiant games."
      ]
    ]

icon : String -> Element msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
  |> html

footerSize = sizeStep -1 |> round
resetSize = sizeStep 2 |> round

sizeStep = modular 16 1.25

windowBackColor = rgb255 21 22 19
buttonBackColor = rgb255 59 64 54
buttonBorderColor = rgb255 255 255 160

displayWindowPoints : List Point -> Element msg
displayWindowPoints points =
  row
    (List.append
      [ alignLeft
      , alignTop
      ]
      (List.map (displayWindowPoint >> inFront) points)
    )
    []

displayWindowPoint : Point -> Element msg
displayWindowPoint (x,y) =
  row
    [ moveRight x
    , moveDown y
    ]
    [ el
      [ width (px 5)
      , height (px 5)
      , Background.color (rgb 1 1 1)
      , alignTop
      ] none
    , text ((printFloat x) ++ "," ++ (printFloat y))
    ]

printFloat : Float -> String
printFloat x =
  (x * 100)
    |> truncate
    |> (\y -> (toFloat y) / 100)
    |> String.fromFloat

atMost : number -> number -> number
atMost = min

atLeast : number -> number -> number
atLeast = max
