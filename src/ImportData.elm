module ImportData exposing (..)

import Console
import Dxf.Decode
import Layout exposing (Layout, GroupId)
import Layout.DecodeDxf as DecodeDxf
import Traits exposing (TraitId, Traits, God(..))
import Traits.Decode as Decode

import Json.Decode as Decode

main : Program () Model Msg
main = Platform.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =
  { traits : Traits
  }

type Msg
  = Exit
  | ConsoleEvent (Result Decode.Error Console.Event)

type File
  = TraitsJson Traits
  | Dxf God Layout

init : () -> (Model, Cmd Msg)
init _ =
  ( { traits = Traits.empty
    }
  , Cmd.batch
    [ Console.write "start"
    , Console.readFile "traits.json"
    ]
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Exit ->
      (model, Console.exit)
    ConsoleEvent (Ok (Console.ReadFile name (Ok contents))) ->
      case parseFile name contents of
        Ok (TraitsJson traits) -> 
          ( {model | traits = traits}
          , traits
            |> Traits.allGods
            |> List.map Traits.dataGod
            |> List.map fetchDxf
            |> Cmd.batch
          )
        Ok (Dxf god layout) ->
          ( { model
            | traits = Traits.addLayout god layout model.traits
            }
          , Cmd.none
          )
        Err err ->
          ( model
          , Cmd.batch
            [ Console.exit
            , Console.write ("Failed to parse " ++ name ++ " : " ++ err)
            ]
          )
    ConsoleEvent (Ok (Console.ReadFile name (Err err))) ->
      (model, Console.write ("Failed to read " ++ name ++ " : " ++ err))
    ConsoleEvent (Err err) ->
      (model, Console.write ("event decode failed " ++ (Decode.errorToString err)))

fetchDxf : God -> Cmd Msg
fetchDxf god =
  Console.readFile ((god |> Traits.godName |> String.toLower) ++ ".dxf")

parseFile : String -> String -> Result String File
parseFile filename contents =
  case filename |> Debug.log "filename" of
    "traits.json" ->
      Decode.decodeString Decode.traits contents
        |> Result.map TraitsJson
        |> Result.mapError Decode.errorToString
    "artemis.dxf" ->
      parseDxf Artemis contents
    "ares.dxf" ->
      parseDxf Ares contents
    "demeter.dxf" ->
      parseDxf Demeter contents
    "aphrodite.dxf" ->
      parseDxf Aphrodite contents
    "hermes.dxf" ->
      parseDxf Hermes contents
    "athena.dxf" ->
      parseDxf Athena contents
    "poseidon.dxf" ->
      parseDxf Poseidon contents
    "zeus.dxf" ->
      parseDxf Zeus contents
    "dionysus.dxf" ->
      parseDxf Dionysus contents
    _ ->
      Err "Unknown file"

parseDxf : God -> String -> Result String File
parseDxf god contents =
  Dxf.Decode.decodeString DecodeDxf.layout contents
    |> Result.map (Dxf god)
    |> Result.mapError Dxf.Decode.errorToString

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Console.signal Console.SigInt Exit
    , Console.event ConsoleEvent
    ]
