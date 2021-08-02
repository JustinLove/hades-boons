module HadesBoons exposing (..)

import BoonChart exposing (DragMode(..))
import Dxf.Decode
import Geometry exposing (Point, tau)
import Layout exposing (Layout, GroupId)
import Layout.DecodeDxf as DecodeDxf
import Log
import Traits exposing (TraitId, SlotId, Traits, God(..), BoonStatus(..))
import Traits.Decode as Decode
import Traits.Generated as Generated
import Texts.Generated as Generated
--import Traits.Stub as Generated
import View

import SuperText exposing (..)
import SuperText.Parser as SuperText
import Parser.Advanced as Parser

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Canvas.Texture as Canvas
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Value)
import Parser.Advanced
import Process
import Set exposing (Set)
import Task
import Time
import Touch
import Url exposing (Url)

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | GotTraits (Result Http.Error Traits)
  | GotTexts (Result Http.Error (Dict String String))
  | GotLayout God (Result Http.Error Layout)
  | WindowSize (Int, Int)
  | WindowReSize (Int, Int)
  | Rotate Float
  | Fade Float
  | HoverHeld TraitId
  | TouchMove Float Float
  | TouchPinch Float

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , windowWidth : Int
  , windowHeight : Int
  , windowKnown : Bool
  , canvasTextures : Dict String Canvas.Texture
  , traits : Traits
  , texts : Dict String String
  , chartMetrics : BoonChart.ChartMetrics
  , activeTraits : Set TraitId
  , activeBasicGroups : List (Set GroupId)
  , activeDuoGroups : Set GroupId
  , activeSlots : Set SlotId
  , metaUpgrade : String
  , boonStatus : Dict TraitId BoonStatus
  , currentPrimaryMenu : Maybe SlotId
  , artAttribution : Bool
  , hoverBoon : Maybe TraitId
  , descriptionBoon : Maybe TraitId
  , descriptionBoonLast : Maybe TraitId
  , descriptionVisibility : Float
  , touchTracking : Touch.Model Msg
  , focusGod : Maybe God
  , rotation : Float
  , drag : DragMode
  , offset : Point
  , zoom : Float
  }

main = Browser.application
  { init = init
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

initialModel : () -> Url -> Navigation.Key -> Model
initialModel flags location key =
  let rotation = tau / 16 in
  { location = location
  , navigationKey = key
  , windowWidth = 320
  , windowHeight = 300
  , windowKnown = False
  , canvasTextures = Dict.empty
  , traits = Traits.empty
  , texts = Dict.empty
  , chartMetrics = BoonChart.calculateMetrics Traits.empty rotation
  , activeTraits = Set.empty
  , activeBasicGroups = []
  , activeDuoGroups = Set.empty
  , activeSlots = Set.empty
  , metaUpgrade = "AmmoMetaUpgrade"
  , boonStatus = Dict.empty
  , currentPrimaryMenu = Nothing
  , artAttribution = False
  , hoverBoon = Nothing
  , descriptionBoon = Nothing
  , descriptionBoonLast = Nothing
  , descriptionVisibility = 0.0
  , touchTracking = Touch.initModel
    [ Touch.onMove {fingers = 1} TouchMove
    , Touch.onPinch TouchPinch
    ]
  , focusGod = Nothing
  , rotation = rotation
  , drag = Released
  , offset = (0,0)
  , zoom = 1
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags location key =
  let model = initialModel flags location key in
  if Generated.traits == Traits.empty then
    initAndLoad model
  else
    initWithGenerated model

superPart texts tooltipData st =
  case st of
    Format f sub ->
      List.concatMap (superPart texts tooltipData) sub
    Keywords (Keyword k) ->
      if Dict.get k texts == Nothing then
        []
      else
      []
    TooltipData (Tooltip t) ->
      if Dict.get t tooltipData == Nothing then
        [t]
      else
        []
    TooltipData (PercentTooltip t) ->
      if Dict.get t tooltipData == Nothing then
        [t]
      else
        []
    _ ->
      []

superCheck model =
  let 
    _ = Traits.allTraits model.traits
      |> List.append Traits.miscBoons
      |> List.concatMap (\trait ->
        Parser.run SuperText.parse trait.description
          |> Result.map (List.concatMap (superPart model.texts trait.tooltipData))
          --|> Result.mapError (Debug.log ("supertext error: " ++ trait.description))
          |> Result.withDefault []
        )
      |> Set.fromList
      --|> Debug.log "data"
  in
    model

initWithGenerated : Model -> (Model, Cmd Msg)
initWithGenerated model =
  ( { model
    | traits = Generated.traits
    , texts = Generated.texts
    }
      --|> superCheck
      |> updateChartMetrics
      |> updateDerivedStatus
  , Cmd.batch
    [ initialWindowSize
    ]
  )

initAndLoad : Model -> (Model, Cmd Msg)
initAndLoad model =
  ( model
  , Cmd.batch
    [ fetchTraits
    , fetchText
    , initialWindowSize
    ]
  )

initialWindowSize : Cmd Msg
initialWindowSize =
  Dom.getViewport
    |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
    |> Task.perform WindowSize

update msg model =
  case msg of
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Navigate (Browser.Internal url) ->
      ( {model | location = url}
      , Navigation.pushUrl model.navigationKey (Url.toString url)
      )
    Navigate (Browser.External url) ->
      (model, Navigation.load url)
    GotTraits (Ok traits) ->
      ( { model
        | traits = traits
        }
          --|> superCheck
          |> updateChartMetrics
          |> updateDerivedStatus
      , traits
        |> Traits.allGods
        |> List.map Traits.dataGod
        |> List.map fetchDxf
        |> Cmd.batch
      )
    GotTraits (Err error) ->
      (model, Log.httpError "fetch error: traits" error)
    GotTexts (Ok texts) ->
      ( { model | texts = texts }
      , Cmd.none
      )
    GotTexts (Err error) ->
      (model, Log.httpError "fetch error: texts" error)
    GotLayout god (Ok layout) ->
      ( { model
        | traits = Traits.addLayout god layout model.traits
        }
          |> updateChartMetrics
          |> updateDerivedStatus
      , Cmd.none
      )
    GotLayout god (Err error) ->
      (model, Log.httpError "fetch error: layout" error)
    WindowSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height, windowKnown = True}
        |> defaultView
      , Cmd.none)
    WindowReSize (width, height) ->
      let
        oldCenter = screenToChart model (View.chartCenter model.windowWidth model.windowHeight)
          |> Geometry.add (-0.5, -0.5)
          |> (\(x,y) -> (x, -y))
      in
      ( { model | windowWidth = width , windowHeight = height, windowKnown = True }
        |> focusView oldCenter (1/model.zoom) model.rotation
      , Cmd.none
      )
    Rotate dt ->
      ( focusFollow Poseidon { model | rotation = Geometry.modAngle (model.rotation + (dt/10000)) }
      , Cmd.none
      )
    Fade dt ->
      ( if model.descriptionBoon /= Nothing then
          let vis = model.descriptionVisibility + (dt / 300) in
          if vis >= 1.0 then
            { model
            | descriptionBoonLast = model.descriptionBoon
            , descriptionVisibility = 1.0
            }
          else
            { model
            | descriptionBoonLast = model.descriptionBoon
            , descriptionVisibility = vis
            }
        else
          let vis = model.descriptionVisibility - (dt / 500) in
          if vis <= 0.0 then
            { model
            | descriptionBoonLast = Nothing
            , descriptionVisibility = 0.0
            }
          else
            { model
            | descriptionVisibility = vis
            }
      , Cmd.none
      )
    HoverHeld id ->
      if Just id == model.hoverBoon then
        ( { model
          | descriptionBoon = Just id
          }
        , Cmd.none
        )
      else
        (model, Cmd.none)
    TouchMove dx dy ->
      ( { model
        | offset = model.offset
          |> Geometry.add (dx, dy)
        }
      , Cmd.none
      )
    TouchPinch pinch ->
      let
        screen = min model.windowWidth model.windowHeight |> toFloat
        tweak = pinch / screen |> clamp -0.2 1.0
        newZoom = model.zoom + model.zoom * tweak |> clamp 0.8 32
        clampedTweak = newZoom / model.zoom
        diff = (View.chartCenter model.windowWidth model.windowHeight) |> Geometry.sub model.offset
      in
      ( { model
        | zoom = newZoom
        , offset = diff
          |> Geometry.sub model.offset
          |> Geometry.add (Geometry.scale clampedTweak diff)
        }
      , Cmd.none
      )
    UI (View.None) ->
      (model, Cmd.none)
    UI (View.OnMouseMove point) ->
      if model.drag == Released then
        let hit = hitBoon model point in
          case hit of
            Just id ->
              ( { model
                | hoverBoon = hit
                }
              , if hit /= model.hoverBoon then
                  Process.sleep 200 |> Task.perform (always (HoverHeld id))
                else
                  Cmd.none
              )
            Nothing ->
              ( { model
                | hoverBoon = Nothing
                , descriptionBoon = Nothing
                }
              , Cmd.none
              )
      else
        ( { model
          | offset = dragTo model.drag point model.offset
          }
        , Cmd.none
        )
    UI (View.OnMouseDown point) ->
      ( case hitBoon model point of
          Just id -> selectBoon id { model | drag = Dragging model.offset point }
          Nothing -> { model | drag = Dragging model.offset point }
      , Cmd.none
      )
    UI (View.OnMouseUp point) ->
      ( { model
        | offset = dragTo model.drag point model.offset
        , drag = Released
        }
      , Cmd.none
      )
    UI (View.OnWheel point scroll) ->
      let
        tweak = if scroll > 0 then 0.8 else 1.2
        newZoom = model.zoom * tweak |> clamp 0.8 32
        clampedTweak = newZoom / model.zoom
        diff = point |> Geometry.sub model.offset
      in
      ( { model
        | zoom = newZoom
        , offset = diff
          |> Geometry.sub model.offset
          |> Geometry.add (Geometry.scale clampedTweak diff)
        }
      , Cmd.none
      )
    UI (View.TextureLoaded key mtexture) ->
      ( { model
        | canvasTextures =
          Dict.update key (always mtexture) model.canvasTextures
        }
      , Cmd.none)
    UI (View.SelectGod god) ->
      (model |> focusOn god, Cmd.none)
    UI (View.ViewAll) ->
      (model |> defaultView, Cmd.none)
    UI (View.SelectSlot slot) ->
      ( { model
        | currentPrimaryMenu =
          if model.currentPrimaryMenu == Just slot then
            Nothing
          else
            Just slot
        , activeTraits = Set.diff model.activeTraits
          (model.traits
            |> Traits.boonsForSlot slot
            |> List.map .trait
            |> Set.fromList
          )
        , metaUpgrade =
          if slot == "Soul" then
            if model.metaUpgrade == "AmmoMetaUpgrade" then
              "ReloadAmmoMetaUpgrade"
            else
              "AmmoMetaUpgrade"
          else
            model.metaUpgrade
        , descriptionBoon =
          if slot == "Soul" then
            if model.metaUpgrade == "AmmoMetaUpgrade" then
               Just "ReloadAmmoMetaUpgrade"
            else
              Just "AmmoMetaUpgrade"
          else
            model.descriptionBoon
        }
          |> updateDerivedStatus
      , Cmd.none
      )
    UI (View.SelectPrimary id slot) ->
      let
        closed =
          { model
          | currentPrimaryMenu = Nothing
          , activeTraits = Set.diff model.activeTraits
            (model.traits
              |> Traits.boonsForSlot slot
              |> List.map .trait
              |> Set.fromList
            )
          }
      in
      ( selectBoon id closed
      , Cmd.none
      )
    UI (View.SelectKeepsake id) ->
      ( { model
        | currentPrimaryMenu = Nothing
        , activeTraits = Set.diff model.activeTraits
          (model.traits
            |> Traits.boonsForSlot "Keepsake"
            |> List.map .trait
            |> Set.fromList
          )
        }
          |> selectBoon id
      , Cmd.none
      )
    UI (View.SelectWeapon id) ->
      ( { model
        | currentPrimaryMenu = Nothing
        , activeTraits = Set.diff model.activeTraits
          (model.traits
            |> Traits.boonsForSlot "Weapon"
            |> List.map .trait
            |> Set.fromList
          )
        }
          |> selectBoon id
      , Cmd.none
      )
    UI (View.OnTrayEnter id) ->
      ( { model
        | hoverBoon = Just id
        }
      , if Just id /= model.hoverBoon then
          Process.sleep 200 |> Task.perform (always (HoverHeld id))
        else
          Cmd.none
      )
    UI (View.OnTrayLeave id) ->
      ( { model
        | hoverBoon = Nothing
        , descriptionBoon = Nothing
        }
      , Cmd.none
      )
    UI (View.Reset) ->
      ( { model | activeTraits = Set.empty }
        |> updateDerivedStatus
      , Cmd.none
      )
    UI (View.Supergiant hover) ->
      ( { model | artAttribution = hover }, Cmd.none )
    UI (View.TouchMsg touchMsg) ->
      Touch.update
        touchMsg
        model.touchTracking
        ( \newTouchModel -> { model | touchTracking = newTouchModel } )

hitBoon : Model -> Point -> Maybe TraitId
hitBoon model point =
  point
    |> screenToChart model
    |> BoonChart.hitChart model.chartMetrics model.zoom

screenToChart : Viewable model -> Point -> Point
screenToChart model point =
  point
    |> Geometry.minus model.offset
    |> Geometry.scale (1/(View.chartDiameter model.windowWidth model.windowHeight))
    |> Geometry.scale (1/model.zoom)

selectBoon : TraitId -> Model -> Model
selectBoon id model =
  { model
  | activeTraits =
    if Set.member id model.activeTraits then
      Set.remove id model.activeTraits
    else
      Set.insert id model.activeTraits
  , descriptionBoon =
    if Set.member id model.activeTraits then
      Nothing
    else
      Just id
  }
    |> updateDerivedStatus

updateDerivedStatus : Model -> Model
updateDerivedStatus model =
  let
    activeTraits = model.activeTraits
      |> updateHadesCall
      |> updatePoseidonDrop
    gods = Traits.allGods model.traits
    activeSlots = Traits.calculateActiveSlots activeTraits model.traits
    slotTraits = Set.map (\slot -> "Any"++slot) activeSlots

    activeLayoutTraits =
      activeTraits
        |> Set.union slotTraits
        |> Set.insert model.metaUpgrade
  in
  { model
  | activeTraits = activeTraits
  , activeBasicGroups = Traits.calculateActiveLayoutGroups activeLayoutTraits gods
  , activeDuoGroups = Traits.calculateActiveDuoSets gods activeTraits (Traits.duoBoons model.traits)
  , activeSlots = activeSlots
  , boonStatus = Traits.traitStatus activeTraits model.metaUpgrade model.traits
  }

updateHadesCall : Set TraitId -> Set TraitId
updateHadesCall activeTraits =
  if Set.member "HadesShoutKeepsake" activeTraits then
    Set.insert "HadesShoutTrait" activeTraits
  else
    Set.remove "HadesShoutTrait" activeTraits

updatePoseidonDrop : Set TraitId -> Set TraitId
updatePoseidonDrop activeTraits =
  if Set.member "RandomMinorLootDrop" activeTraits then
    Set.insert "PoseidonPickedUpMinorLootTrait" activeTraits
  else
    Set.remove "PoseidonPickedUpMinorLootTrait" activeTraits

updateChartMetrics : Model -> Model
updateChartMetrics model =
  { model | chartMetrics = BoonChart.calculateMetrics model.traits model.rotation }

focusOn : God -> Model -> Model
focusOn god model =
  let
    rotation = BoonChart.focusAngleOf model.chartMetrics god
    chartMetrics = BoonChart.calculateMetrics model.traits rotation
    center = BoonChart.focusPositionOf chartMetrics god
  in
  { model
  | focusGod = Just god
  , chartMetrics = chartMetrics
  }
    |> focusView center chartMetrics.adjacentDistance rotation

focusFollow : God -> Model -> Model
focusFollow god model =
  let
    chartMetrics = BoonChart.calculateMetrics model.traits model.rotation
    center = BoonChart.focusPositionOf chartMetrics god
  in
  { model
  | focusGod = Just god
  , chartMetrics = chartMetrics
  }
    |> focusView center chartMetrics.adjacentDistance model.rotation

type alias Viewable r =
  { r
  | windowWidth : Int
  , windowHeight : Int
  , rotation : Float
  , offset : Point
  , zoom : Float
  }

focusView : Point -> Float -> Float -> Viewable model -> Viewable model
focusView center diameter rotation model =
  let
    zoom = 1/diameter
    offset = center
      |> (\(x,y) -> (-x,y))
      |> Geometry.add (-0.5,-0.5)
      |> Geometry.scale (View.chartDiameter model.windowWidth model.windowHeight)
      |> Geometry.scale zoom
      |> Geometry.add (View.chartCenter model.windowWidth model.windowHeight)
  in
  { model
  | rotation = rotation
  , offset = offset
  , zoom = zoom
  }

defaultView : Model -> Model
defaultView =
  focusView (0, 0) 1 (tau/16)
    >> updateChartMetrics

dragTo : DragMode -> Point -> Point -> Point
dragTo drag point oldOffset =
  case drag of
    Released -> oldOffset
    Dragging offset start ->
      start
        |> Geometry.sub point
        |> Geometry.add offset

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize (\w h -> WindowReSize (w, h))
    , if model.windowKnown == False then
       Time.every 1000 (\_ -> WindowSize (model.windowWidth, model.windowHeight))
      else
        Sub.none
    --, Browser.Events.onAnimationFrameDelta Rotate
    , if (model.descriptionBoon /= Nothing && model.descriptionVisibility < 1.0) || (model.descriptionBoon == Nothing && model.descriptionVisibility > 0.0) || model.descriptionBoon /= model.descriptionBoonLast then
        Browser.Events.onAnimationFrameDelta Fade
      else
        Sub.none
    ]

fetchTraits : Cmd Msg
fetchTraits =
  Http.get
    { url = "../data/traits.json"
    , expect = Http.expectJson GotTraits Decode.traits
    }

fetchText : Cmd Msg
fetchText =
  Http.get
    { url = "../data/en.json"
    , expect = Http.expectJson GotTexts Decode.texts
    }

fetchDxf : God -> Cmd Msg
fetchDxf god =
  Http.get
    { url = "../data/" ++ (god |> Traits.godName |> String.toLower) ++ ".dxf"
    , expect = expectDxf (GotLayout god) DecodeDxf.layout
    }

expectDxf : (Result Http.Error a -> msg) -> Dxf.Decode.Decoder a -> Http.Expect msg
expectDxf tagger decoder =
  Http.expectString (receiveDxf decoder >> tagger)

receiveDxf : Dxf.Decode.Decoder a -> Result Http.Error String -> Result Http.Error a
receiveDxf decoder result =
  result
    |> Result.andThen (Dxf.Decode.decodeString decoder >> Result.mapError (Dxf.Decode.errorToString >> Http.BadBody))
