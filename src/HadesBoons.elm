module HadesBoons exposing (..)

--import MeasureText
import View

import Browser
--import Browser.Dom as Dom
--import Browser.Events
import Browser.Navigation as Navigation
import Url exposing(Url)

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  --| WindowSize (Int, Int)
  --| TextSize MeasureText.TextSize

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
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
    }
  , Cmd.none
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
    --WindowSize (width, height) ->
     -- ( {model | windowWidth = width, windowHeight = height}, Cmd.none)
    --TextSize {text, width} ->
      --( {model | labelWidths = Dict.insert text (width/100) model.labelWidths}, Cmd.none)
    UI (View.None) ->
      ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
    --, Browser.Events.onResize (\w h -> WindowSize (w, h))
    --, MeasureText.textSize TextSize
    ]
