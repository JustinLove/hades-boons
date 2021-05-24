module HadesBoons exposing (..)

import BoonChart exposing (DragMode(..))
import Dxf.Decode
import Geometry exposing (Point)
import Layout exposing (Layout, GroupId)
import Layout.DecodeDxf as DecodeDxf
import Log
--import MeasureText
import Traits exposing (TraitId, Traits, God(..), BoonStatus(..))
import Traits.Decode as Decode
import Traits.Generated
import View

import Browser
--import Browser.Dom as Dom
--import Browser.Events
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Value)
import Parser.Advanced
import Set exposing (Set)
import Url exposing (Url)

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | GotTraits (Result Http.Error Traits)
  | GotLayout God (Result Http.Error Layout)
  --| WindowSize (Int, Int)
  --| TextSize MeasureText.TextSize

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , traits : Traits
  , chartMetrics : BoonChart.ChartMetrics
  , activeTraits : Set TraitId
  , activeGroups : Set GroupId
  , boonStatus : Dict TraitId BoonStatus
  , drag : DragMode
  , offset : Point
  , zoom : Float
  }

main = Browser.application
  { init = initWithGenerated
  --{ init = initAndLoad
  , view = View.document UI
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = Navigate
  , onUrlChange = CurrentUrl
  }

initialModel : () -> Url -> Navigation.Key -> Model
initialModel flags location key =
  { location = location
  , navigationKey = key
  --, windowWidth = 320
  --, windowHeight = 300
  --, labelWidths = Dict.empty
  , traits = Traits.empty
  , chartMetrics = BoonChart.calculateMetrics Traits.empty
  , activeTraits = Set.empty
  , activeGroups = Set.empty
  , boonStatus = Dict.empty
  , drag = Released
  , offset = (0,0)
  , zoom = 0.15
  }

initWithGenerated : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
initWithGenerated flags location key =
  let model = initialModel flags location key in
  ( { model
    | traits = Traits.Generated.traits
    }
      |> updateChartMetrics
      |> updateDerivedStatus
  , Cmd.batch
    [
    ]
  )

initAndLoad : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
initAndLoad flags location key =
  let model = initialModel flags location key in
  ( model
  , Cmd.batch
    [ fetchTraits
    ]
  )

    --, Dom.getViewport
      --|> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      --|> Task.perform WindowSize

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
    --WindowSize (width, height) ->
     -- ( {model | windowWidth = width, windowHeight = height}, Cmd.none)
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

hitBoon : Model -> Point -> Maybe TraitId
hitBoon model point =
  point
    |> Geometry.add (-8, -8)
    |> BoonChart.hitChart model.chartMetrics model.offset model.zoom

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
  { model
  | activeGroups = Traits.calculateActiveGroups model.activeTraits model.traits
  , boonStatus = Traits.traitStatus model.activeTraits model.traits
  }

updateChartMetrics : Model -> Model
updateChartMetrics model =
  { model | chartMetrics = BoonChart.calculateMetrics model.traits }

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
    [
    --, Browser.Events.onResize (\w h -> WindowSize (w, h))
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
