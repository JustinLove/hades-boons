module HadesBoons exposing (..)

import BoonChart exposing (DragMode(..))
import Dxf.Parser as Parser
import Geometry exposing (Point)
import Layout exposing (..)
import Layout.Decode as Decode
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
import Parser.Advanced
import Url exposing(Url)
import Xml.Decode

type Msg
  = UI (View.Msg)
  | CurrentUrl Url
  | Navigate Browser.UrlRequest
  | GotTraits (Result Http.Error Traits)
  | GotLayout (Result Http.Error Layout)
  | GotDxf (Result Http.Error Parser.Dxf)
  --| WindowSize (Int, Int)
  --| TextSize MeasureText.TextSize

type alias Model =
  { location : Url
  , navigationKey : Navigation.Key
  , traits : Traits
  , layout : Layout
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
    , layout = []
    , traits = []
    , drag = Released
    , offset = (0,0)
    , zoom = 0.15
    }
  , Cmd.batch
    [ fetchTraits
    , fetchLayout
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
      ({model | traits = identifyBoons traits}, Cmd.none)
    GotTraits (Err error) ->
      (model, Log.httpError "fetch error: traits" error)
    GotLayout (Ok layout) ->
      ({model | layout = layout}, Cmd.none)
    GotLayout (Err error) ->
      (model, Log.httpError "fetch error: layout" error)
    GotDxf (Ok dxf) ->
      let
        _ = Debug.log "dxf" dxf
        _ = dxf
          |> List.map (\s -> case s of
            Parser.Section name values ->
              let
                _ = Debug.log "sec" name
                _ = values
                  |> List.map (\v -> case v of
                    Parser.UnknownCode code value ->
                      let _ = Debug.log value code in v
                    Parser.EntityType (Parser.UnknownType t) ->
                      let _ = Debug.log "unknown type" t in v
                    _ -> v
                    )
              in name
            )
      in
      (model, Cmd.none)
    GotDxf (Err error) ->
      (model, Log.httpError "fetch error: dxf" error)
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

fetchLayout : Cmd Msg
fetchLayout =
  Http.get
    { url = "demeter.xml"
    , expect = expectXml GotLayout Decode.layout
    }

expectXml : (Result Http.Error a -> msg) -> Xml.Decode.Decoder a -> Http.Expect msg
expectXml tagger decoder =
  Http.expectString (receiveXml decoder >> tagger)

receiveXml : Xml.Decode.Decoder a -> Result Http.Error String -> Result Http.Error a
receiveXml decoder result =
  result
    |> Result.andThen (Xml.Decode.run decoder >> Result.mapError Http.BadBody)

fetchDxf : Cmd Msg
fetchDxf =
  Http.get
    { url = "qcad.dxf"
    , expect = expectDxf GotDxf Parser.dxf
    }

expectDxf : (Result Http.Error a -> msg) -> Parser.DxfParser a -> Http.Expect msg
expectDxf tagger parser =
  Http.expectString (receiveDxf parser >> tagger)

receiveDxf : Parser.DxfParser a -> Result Http.Error String -> Result Http.Error a
receiveDxf parser result =
  result
    |> Result.andThen (Parser.Advanced.run parser >> Result.mapError (Parser.deadEndsToString >> Http.BadBody))
