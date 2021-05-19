module HadesBoons exposing (..)

import BoonChart exposing (DragMode(..))
import Dxf.Decode
import Geometry exposing (Point)
import Layout exposing (..)
import Layout.DecodeDxf as DecodeDxf
import Log
--import MeasureText
import Traits exposing (TraitId, Traits)
import Traits.Decode as Decode
import View

import Browser
--import Browser.Dom as Dom
--import Browser.Events
import Browser.Navigation as Navigation
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
  | GotLayout (Result Http.Error Layout)
  --| WindowSize (Int, Int)
  --| TextSize MeasureText.TextSize

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , traits : Traits
  , layout : Layout
  , activeTraits : Set TraitId
  , activeGroups : Set GroupId
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

init : () -> Url -> Navigation.Key -> (Model, Cmd Msg)
init flags location key =
  ( { location = location
    , navigationKey = key
    --, windowWidth = 320
    --, windowHeight = 300
    --, labelWidths = Dict.empty
    , layout = Layout [] []
    , traits = Traits.empty
    , activeTraits = Set.empty
    , activeGroups = Set.empty
    , drag = Released
    , offset = (0,0)
    , zoom = 0.15
    }
  , Cmd.batch
    [ fetchTraits
    , fetchDxf
    ]
    --, Dom.getViewport
      --|> Task.map (\viewport -> (round viewport.viewport.width, round viewport.viewport.height))
      --|> Task.perform WindowSize

      --|> List.map (\name -> MeasureText.getTextWidth {font = "100px sans-serif", text = name})
  )

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
      ({model | traits = traits}, Cmd.none)
    GotTraits (Err error) ->
      (model, Log.httpError "fetch error: traits" error)
    GotLayout (Ok layout) ->
      ({model | layout = layout}, Cmd.none)
    GotLayout (Err error) ->
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
      ( { model | drag = Dragging model.offset point }, Cmd.none)
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
    UI (View.SelectedBoon id) ->
      ( { model
        | activeTraits =
          if Set.member id model.activeTraits then
            Set.remove id model.activeTraits
          else
            Set.insert id model.activeTraits
        }
          |> updateActiveGroups
      , Cmd.none
      )

updateActiveGroups : Model -> Model
updateActiveGroups model =
  { model | activeGroups = calculateActiveGroups model.layout model.activeTraits }

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

fetchDxf : Cmd Msg
fetchDxf =
  Http.get
    { url = "demeter.dxf"
    , expect = expectDxf GotLayout DecodeDxf.layout
    }

expectDxf : (Result Http.Error a -> msg) -> Dxf.Decode.Decoder a -> Http.Expect msg
expectDxf tagger decoder =
  Http.expectString (receiveDxf decoder >> tagger)

receiveDxf : Dxf.Decode.Decoder a -> Result Http.Error String -> Result Http.Error a
receiveDxf decoder result =
  result
    |> Result.andThen (Dxf.Decode.decodeString decoder >> Result.mapError (Dxf.Decode.errorToString >> Http.BadBody))
