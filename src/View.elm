module View exposing (Msg(..), document, view, chartDiameter, chartCenter)

import BoonChart
--import BoonChart.Svg
import BoonChart.Canvas
import Geometry
import MouseWheel
import SuperText exposing (..)
import SuperText.Parser as SuperText
import Traits exposing (TraitId, God, Trait, SlotId, BoonStatus(..))

import Dict exposing (Dict)
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
import Html.Events
import Json.Decode as Decode
import Parser.Advanced as Parser
import Set exposing (Set)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Touch

type alias Point = (Float, Float)

type Msg
  = None
  | OnMouseMove Point
  | OnMouseDown Point
  | OnMouseUp Point
  | OnWheel Point Int
  | TextureLoaded String (Maybe Canvas.Texture)
  | SelectGod God
  | ViewAll
  | SelectSlot SlotId
  | SelectPrimary TraitId SlotId
  | SelectKeepsake TraitId
  | SelectWeapon TraitId
  | OnTrayEnter TraitId
  | OnTrayLeave TraitId
  | Reset
  | Supergiant Bool
  | TouchMsg Touch.Msg

type Frame
  = Primary
  | Common
  | Duo
  | Keepsake
  | Legendary
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
      , inFront (displayDescription model)
      , inFront (displayFooter model.artAttribution)
      , inFront (displayGodSelect model)
      , inFront (displaySlotSelect model)
      , inFront displayReset
      , behindContent (touchEvents model.drag)
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
          , onTexture = TextureLoaded
          , drag = model.drag
          , offset = model.offset
          , zoom = model.zoom
          , diameter = chartDiameter model.windowWidth model.windowHeight
          , width = model.windowWidth//1
          , height = model.windowHeight
          , textures = model.canvasTextures
          }
          |> html
        else
          none
      ]


touchEvents : BoonChart.DragMode -> Element Msg
touchEvents drag =
  Touch.element
    [ MouseWheel.onWheel OnWheel
    , when (drag == BoonChart.Released) (Html.Events.on "mousedown" (mouseDecoder OnMouseDown))
    , when (drag /= BoonChart.Released) (Html.Events.on "mouseup" (mouseDecoder OnMouseUp))
    , when (drag /= BoonChart.Released) (Html.Events.on "mouseleave" (mouseDecoder OnMouseUp))
    , (Html.Events.on "mousemove" (mouseDecoder OnMouseMove))
    , Html.Attributes.style "width" "100vw"
    , Html.Attributes.style "height" "100vh"
    ] 
    TouchMsg
    |> html

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
    button = (traitSelectButton (SelectSlot slot) None None scaled)
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
        (boonIcon scaled (if slot == "Keepsake" then Keepsake else Common) (Just boon.icon) boon.name button)
      Nothing ->
        (boonIcon scaled (if slot == "Keepsake" then Keepsake else Primary) mpath desc button)
    )

keepsakeTrayButton model scaled =
  let
    inSlot =
      model.traits
        |> Traits.boonsForSlot "Keepsake"
        |> List.filter (\{trait} -> Set.member trait model.activeTraits)
        |> List.head
    button = traitSelectButton (SelectSlot "Keepsake") None None scaled
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
        (keepsakeIcon scaled (Just boon.icon) boon.name Active button)
      Nothing ->
        (keepsakeIcon scaled Nothing "Keepsake" Active button)
    )

metaTrayButton model scaled =
  let
    inSlot =
      Traits.miscBoons
        |> List.filter (Traits.isSlot "Soul")
        |> List.filter (\{trait} -> trait == model.metaUpgrade)
        |> List.head
    button = traitSelectButton (SelectSlot "Soul") None None scaled
  in
  el
    [ width (px (scaled 200))
    ]
    (case inSlot of
      Just boon ->
        (metaIcon scaled MetaUpgrade (Just boon.icon) boon.name button)
      Nothing ->
        (metaIcon scaled Tray Nothing "Soul" button)
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
      |> List.map (\boon -> keepsakeIcon scaled (Just boon.icon) (boon.name) ((if Set.intersect boon.requiredFalseTraits model.activeTraits |> Set.isEmpty then Available else Excluded)) (traitSelectButton (SelectKeepsake boon.trait) (OnTrayEnter boon.trait) (OnTrayLeave boon.trait) scaled))
    )

weaponMenu model scaled =
  row
    [ centerY
    ]
    (Traits.miscBoons
      |> List.filter (Traits.isSlot "Weapon")
      |> List.map (\boon -> boonIcon scaled Common (Just boon.icon) (boon.name) (traitSelectButton (SelectWeapon boon.trait) (OnTrayEnter boon.trait) (OnTrayLeave boon.trait) scaled))
    )

boonSelectButton : (Float -> Int) -> SlotId -> God -> Traits.Trait -> Element Msg
boonSelectButton scaled slot god boon =
  el
    [ inFront
      (el [ alignBottom, alignRight ] (displayGodIcon scaled (god |> Traits.godIcon) (god |> Traits.godName)))
    ]
    (boonIcon scaled Common (Just boon.icon) boon.name (traitSelectButton (SelectPrimary boon.trait slot) (OnTrayEnter boon.trait) (OnTrayLeave boon.trait) scaled))

boonIcon : (Float -> Int) -> Frame -> Maybe String -> String -> Element msg -> Element msg
boonIcon scaled frame mpath desc content =
  el
    [ width (px (scaled 201))
    , height (px (scaled 187))
    , padding (scaled -100)
    , behindContent (displayFrame scaled frame)
    , behindContent (boonIconImage scaled mpath desc)
    ]
    content

keepsakeIcon : (Float -> Int) -> Maybe String -> String -> BoonStatus -> Element msg -> Element msg
keepsakeIcon scaled mpath desc status content =
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
        (boonIconImage keepsakeScale mpath desc)
      )
    , if status == Excluded then
        inFront (lockedIcon keepsakeScale)
      else
        spacing 0
    ]
    (if status == Excluded then
      none
    else
      content
    )

metaIcon : (Float -> Int) -> Frame -> Maybe String -> String -> Element msg -> Element msg
metaIcon scaled frame mpath desc content =
  el
    [ width (px (scaled 201))
    , height (px (scaled 207))
    , padding (scaled -100)
    , behindContent (displayFrame scaled frame)
    , behindContent (metaIconImage scaled mpath desc)
    ]
    content

traitSelectButton : msg -> msg -> msg -> (Float -> Int) -> Element msg
traitSelectButton select enter leave scaled =
  Input.button
    [ Border.width (scaled 80)
    , Border.rounded (scaled 80)
    , Border.color invisibleColor
    , centerX
    , centerY
    , Events.onMouseEnter enter
    , Events.onMouseLeave leave
    ]
    { onPress = Just select
    , label = none
    }

boonIconImage : (Float -> Int) -> Maybe String -> String -> Element msg
boonIconImage scaled mpath desc =
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

metaIconImage : (Float -> Int) -> Maybe String -> String -> Element msg
metaIconImage scaled mpath desc =
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
    Duo ->
      el
        [ Background.image "GUI/Screens/BoonIconFrames/duo.png"
        , width (px (scaled 211))
        , height (px (scaled 205))
        , moveUp (scaled 1 |> toFloat)
        , moveRight (scaled 6 |> toFloat)
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
    Legendary ->
      el
        [ Background.image "GUI/Screens/BoonIconFrames/legendary.png"
        , width (px (scaled 211))
        , height (px (scaled 205))
        , moveUp (scaled 1 |> toFloat)
        , moveRight (scaled 4 |> toFloat)
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
  case model.descriptionBoonLast of
    Just id ->
      let
        mboon = Traits.findBoon model.traits id
        desc =
          mboon
            |> Maybe.map (superText model.texts)
            |> Maybe.withDefault []
        title =
          mboon
            |> Maybe.map .name
            |> Maybe.withDefault ""
        backing =
          case mboon of
            Just boon ->
              case boon.frame of
                Traits.DuoFrame -> duoColor
                Traits.LegendaryFrame -> legendryColor
                _ -> windowBackColor
            Nothing ->
              windowBackColor
        titleColor =
          case mboon of
            Just boon ->
              case boon.frame of
                Traits.DuoFrame -> duoColor
                Traits.LegendaryFrame -> legendryColor
                _ -> rgba 1 1 1 1
            Nothing ->
              rgba 1 1 1 1
        scaled = (\x -> x/2 |> floor)
      in
      el
        [ paddingEach
          { top = 0
          , right = resetSize * 5
          , bottom = footerSize * 2 + 2
          , left = 200
          }
        , alignBottom
        , centerX
        , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
        , alpha model.descriptionVisibility
        ]
        ( row
          [ Background.color windowBackColor
          , Border.color buttonBorderColor
          , Border.width 2
          , Border.rounded 2
          , padding 10
          , spacing 10
          , htmlAttribute <| Html.Attributes.style "pointer-events" "auto"
          ]
          [ el
            [ behindContent
              (el
                [ htmlAttribute <| Html.Attributes.class "boonbacking"
                , Border.color backing
                , Border.widthEach
                  { top = 100
                  , bottom = 0
                  , right = 0
                  , left = 130
                  }
                ] none
              )
            ]
            ( case mboon of
              Just boon ->
                case boon.slot of
                  Just "Soul" -> metaIcon scaled MetaUpgrade (Just boon.icon) boon.name none
                  Just "Keepsake" -> keepsakeIcon scaled (Just boon.icon) boon.name Available none
                  _ -> boonIcon scaled (viewFrame boon.frame) (Just boon.icon) boon.name none
              Nothing ->
                none
            )
          , column [ width fill, alignTop, spacing 10 ]
            [ el
              [ Font.size titleSize
              , Font.color titleColor
              ]
              (text title)
            , paragraph
              [ Font.size descriptionSize
              , Font.color descriptionColor
              ]
              desc
            ]
          ]
        )
    Nothing ->
      none

viewFrame : Traits.Frame -> Frame
viewFrame frame =
  case frame of
    Traits.CommonFrame -> Common
    Traits.DuoFrame -> Duo
    Traits.KeepsakeFrame -> Keepsake
    Traits.LegendaryFrame -> Legendary

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
  row
    [ Region.footer
    , spacing 10
    , alignBottom
    , padding 8
    , Font.size footerSize
    , width fill
    , Background.color (rgba 0 0 0 0.5)
    ]
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
        , label = row [ Font.color superGiantColor ] [ icon "star-full", text "Supergiant Games" ]
        }
      ]
    ]

supergiantAttribution : Element msg
supergiantAttribution =
  column
    [ padding (sizeStep -1 |> round)
    , Background.color (rgba 0 0 0 0.5)
    ---, htmlAttribute <| Html.Attributes.style "pointer-events" "none"
    ]
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
descriptionSize = sizeStep 2 |> round
titleSize = sizeStep 3 |> round

sizeStep = modular 16 1.25

windowBackColor = rgb255 21 22 19
buttonBackColor = rgb255 59 64 54
buttonBorderColor = rgb255 255 255 160
invisibleColor = rgba 1 0 0 0
superGiantColor = rgb 202 0 0
descriptionColor = rgb255 160 160 160
boldColor = rgb255 210 210 210
boldGraftColor = rgb255 210 210 210
statColor = rgb255 160 190 160
upgradeColor = rgb255 115 199 69
penaltyColor = rgb255 199 15 15
rareColor = rgb255 0 138 255
duoColor = rgb255 210 255 97
legendryColor = rgb255 255 144 0

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

superText : Dict String String -> Trait -> List (Element msg)
superText texts trait =
  Parser.run SuperText.parse trait.description
    |> Result.map (List.map (superPart texts trait.tooltipData))
    --|> Result.mapError (Debug.log "supertext error")
    |> Result.withDefault [ text trait.description ]

superPart : Dict String String -> Dict String Float -> SuperText -> Element msg
superPart texts tooltipData st =
  case st of
    Text s ->
      text s
    Format f sub ->
      paragraph (superFormat f) (List.map (superPart texts tooltipData) sub)
    Icons i ->
      superIcon i
    Keywords (Keyword k) ->
      el
        [ Font.bold
        , Font.color (rgba 1 1 1 1)
        ]
        (text (Dict.get k texts |> Maybe.withDefault k))
    TempTextData t ->
      superTip tooltipData t
    TooltipData t ->
      superTip tooltipData t

superTip : Dict String Float -> Tooltip -> Element msg
superTip tooltipData tooltip =
  case tooltip of
    PercentTooltip t ->
      Dict.get t tooltipData
        |> Maybe.map (\x -> if x < 0 then String.fromFloat x else "+" ++ (String.fromFloat x))
        |> Maybe.withDefault "+X"
        |> (\x -> x ++ "%")
        |> text
        |> el [ Font.bold ]
    Tooltip t ->
      Dict.get t tooltipData
        |> Maybe.map String.fromFloat
        |> Maybe.withDefault "X"
        |> text
        |> el [ Font.bold ]

superFormat : Format -> List (Attribute msg)
superFormat format =
  case format of
    AltPenalty ->
      [ Font.bold
      , Font.color penaltyColor
      ]
    AltUpgrade ->
      [ Font.bold
      , Font.color upgradeColor
      ]
    Bold ->
      [ Font.extraBold
      , Font.color boldColor
      ]
    BoldGraft ->
      [ Font.extraBold
      , Font.color boldGraftColor
      ]
    Italic ->
      [ Font.italic ]
    PreviousFormat -> []
    RareFormat ->
      [ Font.bold
      , Font.color rareColor
      ]
    StatFormat ->
      [ Font.color statColor ]
    TooltipUpgrade ->
      [ Font.bold
      , Font.color upgradeColor
      , Font.shadow
        { offset = (0,4)
        , blur = 0
        , color = rgba 0 0 0 1
        }
      ]
    Upgrade ->
      [ Font.bold
      , Font.color upgradeColor
      , Font.shadow
        { offset = (3,3)
        , blur = 3
        , color = rgba 0 0 0 1
        }
      ]

superIcon : Icon -> Element msg
superIcon i =
  case i of
    Ammo ->
      superImage
        { src = "GUI/Icons/Ammo_Small.png"
        , description = "Bloodstone"
        }
    Currency ->
      superImage
        { src = "GUI/Icons/Currency_Small.png"
        , description = "Charon's Obol"
        }
    Gem ->
      superImage
        { src = "GUI/Icons/Gems_Small.png"
        , description = "Gemstones"
        }
    GiftPoint ->
      superImage
        { src = "GUI/Icons/Gift_Small.png"
        , description = "Nectar"
        }
    Health ->
      superImage
        { src = "GUI/Icons/Life_Small.png"
        , description = "Life"
        }
    HealthRestore ->
      superImage
        { src = "GUI/Icons/LifeRestore_Small.png"
        , description = "Healing"
        }
    HealthUp ->
      superImage
        { src = "GUI/Icons/LifeUp_Small.png"
        , description = "Max Life"
        }
    KEnemyHealth ->
      superImage
        { src = "GUI/Icons/Life_Small.png"
        , description = "Enemy Health"
        }
    MetaPoint ->
      superImage
        { src = "GUI/Icons/Darkness_Small.png"
        , description = "Darkness"
        }

superImage : { src : String, description : String } -> Element msg
superImage props =
  el
    [ moveDown 2
    , htmlAttribute <| Html.Attributes.class "superImage"
    ]
    (image
      [ height (px descriptionSize)
      ]
      props
    )

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

when : Bool -> Html.Attribute msg -> Html.Attribute msg
when test att =
  if test then att else Html.Attributes.class ""

mouseDecoder tagger =
  (Decode.map tagger clientDecoder)

clientDecoder : Decode.Decoder Point
clientDecoder =
  Decode.map2 Tuple.pair
    (Decode.field "clientX" Decode.float)
    (Decode.field "clientY" Decode.float)
