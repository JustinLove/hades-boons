module HadesBoons exposing (..)

import BoonChart exposing (DragMode(..))
import Dxf.Decode
import Geometry exposing (Point, tau)
import Layout exposing (Layout, GroupId)
import Layout.DecodeDxf as DecodeDxf
import Log
--import MeasureText
import Traits exposing (TraitId, SlotId, Traits, God(..), BoonStatus(..))
import Traits.Decode as Decode
import Traits.Generated as Generated
--import Traits.Stub as Generated
import View

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Value)
import Parser.Advanced
import Set exposing (Set)
import Task
import Url exposing (Url)

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | GotTraits (Result Http.Error Traits)
  | GotLayout God (Result Http.Error Layout)
  | WindowSize (Int, Int)
  | WindowReSize (Int, Int)
  --| TextSize MeasureText.TextSize

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , windowWidth : Int
  , windowHeight : Int
  , traits : Traits
  , chartMetrics : BoonChart.ChartMetrics
  , activeTraits : Set TraitId
  , activeBasicGroups : List (Set GroupId)
  , activeDuoGroups : Set GroupId
  , activeSlots : Set SlotId
  , boonStatus : Dict TraitId BoonStatus
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
  let rotation = -tau / 16 in
  { location = location
  , navigationKey = key
  , windowWidth = 320
  , windowHeight = 300
  --, labelWidths = Dict.empty
  , traits = Traits.empty
  , chartMetrics = BoonChart.calculateMetrics Traits.empty rotation
  , activeTraits = Set.empty
  , activeBasicGroups = []
  , activeDuoGroups = Set.empty
  , activeSlots = Set.empty
  , boonStatus = Dict.empty
  , focusGod = Nothing
  , rotation = rotation
  , drag = Released
  , offset = (0,0)
  , zoom = 0.15
  }

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags location key =
  let model = initialModel flags location key in
  if Generated.traits == Traits.empty then
    initAndLoad model
  else
    initWithGenerated model

initWithGenerated : Model -> (Model, Cmd Msg)
initWithGenerated model =
  ( { model
    | traits = Generated.traits
    }
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
    , initialWindowSize
    ]
  )

initialWindowSize : Cmd Msg
initialWindowSize =
  Dom.getViewport
    |> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
    |> Task.perform WindowSize

      --|> List.map (\name -> MeasureText.getTextWidth {font = "100px sans-serif", text = name})

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
    GotLayout god (Ok layout) ->
      ( { model
        | traits = Traits.addLayout god layout model.traits
        }
          |> updateChartMetrics
      , Cmd.none
      )
    GotLayout god (Err error) ->
      (model, Log.httpError "fetch error: layout" error)
    WindowSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height}
        |> defaultView
      , Cmd.none)
    WindowReSize (width, height) ->
      ( {model | windowWidth = width, windowHeight = height}
      , Cmd.none)
    --TextSize {text, width} ->
      --( {model | labelWidths = Dict.insert text (width/100) model.labelWidths}, Cmd.none)
    UI (View.OnMouseMove point) ->
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
        diff = point |> Geometry.sub model.offset
      in
      ( { model
        | zoom = model.zoom * tweak
        , offset = diff
          |> Geometry.sub model.offset
          |> Geometry.add (Geometry.scale tweak diff)
        }
      , Cmd.none
      )
    UI (View.SelectGod god) ->
      (model |> focusOn god, Cmd.none)
    UI (View.SelectPrimary slot god) ->
      ( model.traits
        |> Traits.boonsOf god
        |> List.filter (Traits.isSlot slot)
        |> List.map (.trait)
        |> List.head
        |> Maybe.map (\id -> selectBoon id model)
        |> Maybe.withDefault model
      , Cmd.none
      )
    UI (View.ViewAll) ->
      (model |> defaultView, Cmd.none)

hitBoon : Model -> Point -> Maybe TraitId
hitBoon model point =
  point
    |> Geometry.add (Geometry.scale -1 model.offset)
    |> Geometry.scale (1/View.chartSize)
    |> Geometry.scale (1/model.zoom)
    |> BoonChart.hitChart model.chartMetrics model.zoom

selectBoon : TraitId -> Model -> Model
selectBoon id model =
  { model
  | activeTraits =
    if Set.member id model.activeTraits then
      Set.remove id model.activeTraits
    else
      Set.insert id model.activeTraits
  }
    |> updateDerivedStatus

updateDerivedStatus : Model -> Model
updateDerivedStatus model =
  let
    gods = Traits.allGods model.traits
    activeSlots = Traits.calculateActiveSlots model.activeTraits model.traits
  in
  { model
  | activeBasicGroups = Traits.calculateActiveLayoutGroups model.activeTraits gods
  , activeDuoGroups = Traits.calculateActiveDuoSets gods model.activeTraits (Traits.duoBoons model.traits)
  , activeSlots = activeSlots
  , boonStatus = Traits.traitStatus model.activeTraits model.traits
  }

updateChartMetrics : Model -> Model
updateChartMetrics model =
  { model | chartMetrics = BoonChart.calculateMetrics model.traits model.rotation }

focusOn : God -> Model -> Model
focusOn god model =
  let
    rotation = BoonChart.focusAngleOf model.chartMetrics god |> Debug.log "rotation"
    chartMetrics = BoonChart.calculateMetrics model.traits rotation
    center = BoonChart.focusPositionOf chartMetrics god
  in
  { model
  | focusGod = Just god
  , chartMetrics = chartMetrics
  }
    |> focusView center chartMetrics.adjacentDistance rotation

focusView : Point -> Float -> Float -> Model -> Model
focusView center diameter rotation model =
  let
    size = diameter * View.chartSize
    widthScale = (toFloat model.windowWidth-16) / size
    heightScale = (toFloat model.windowHeight-56) / size
    zoom = min widthScale heightScale
    offset = center
      |> Geometry.add (-0.5,-0.5)
      |> Geometry.scale View.chartSize
      |> Geometry.scale zoom
      |> Geometry.add ((toFloat model.windowWidth)/2, (toFloat model.windowHeight+40)/2)
  in
  { model
  | rotation = rotation
  , offset = offset
  , zoom = zoom
  }

defaultView : Model -> Model
defaultView =
  focusView (0, 0) 1 (-tau/16)
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
    --, MeasureText.textSize TextSize
    ]

fetchTraits : Cmd Msg
fetchTraits =
  Http.get
    { url = "traits.json"
    , expect = Http.expectJson GotTraits Decode.traits
    }

fetchDxf : God -> Cmd Msg
fetchDxf god =
  Http.get
    { url = (god |> Traits.godName |> String.toLower) ++ ".dxf"
    , expect = expectDxf (GotLayout god) DecodeDxf.layout
    }

expectDxf : (Result Http.Error a -> msg) -> Dxf.Decode.Decoder a -> Http.Expect msg
expectDxf tagger decoder =
  Http.expectString (receiveDxf decoder >> tagger)

receiveDxf : Dxf.Decode.Decoder a -> Result Http.Error String -> Result Http.Error a
receiveDxf decoder result =
  result
    |> Result.andThen (Dxf.Decode.decodeString decoder >> Result.mapError (Dxf.Decode.errorToString >> Http.BadBody))
