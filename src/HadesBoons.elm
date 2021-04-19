module HadesBoons exposing (..)

import BoonChart exposing (DragMode(..))
import Log
--import MeasureText
import Traits exposing (..)
import Traits.Decode as Decode
import View

import Browser
--import Browser.Dom as Dom
--import Browser.Events
import Browser.Navigation as Navigation
import Http
import Json.Decode as Decode exposing (Value)
import Url exposing(Url)

type alias Point = (Float, Float)

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | GotTraits (Result Http.Error Traits)
  --| WindowSize (Int, Int)
  --| TextSize MeasureText.TextSize

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , traits : Traits
  , drag : DragMode
  , offset : Point
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
    , traits = []
    , drag = Released
    , offset = (0,0)
    }
  , fetchTraits
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
    --WindowSize (width, height) ->
     -- ( {model | windowWidth = width, windowHeight = height}, Cmd.none)
    --TextSize {text, width} ->
      --( {model | labelWidths = Dict.insert text (width/100) model.labelWidths}, Cmd.none)
    UI (View.OnMouseMove point) ->
      --let _ = Debug.log "move" point in
      ( { model
        | offset = dragTo model.drag point
        }
      , Cmd.none
      )
    UI (View.OnMouseDown point) ->
      let _ = Debug.log "down" point in
      ( { model | drag = Dragging model.offset point }, Cmd.none)
    UI (View.OnMouseUp point) ->
      let _ = Debug.log "up" point in
      ( { model
        | drag = Released
        }
      , Cmd.none
      )

sub : Point -> Point -> Point
sub (ax, ay) (bx, by) =
  (ax - bx, ay - by)

add : Point -> Point -> Point
add (ax, ay) (bx, by) =
  (ax + bx, ay + by)

fromStart : DragMode -> Point -> Point
fromStart drag point =
  case drag of
    Released -> (0,0)
    Dragging _ start -> start |> sub point

dragTo : DragMode -> Point -> Point
dragTo drag point =
  case drag of
    Released -> (0,0)
    Dragging offset start ->
      start
        |> sub point
        |> add offset

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
