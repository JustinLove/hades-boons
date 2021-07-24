module ImportData exposing (..)

import Console
import Dxf.Decode
import Layout exposing (Layout, GroupId)
import Layout.DecodeDxf as DecodeDxf
import Traits exposing (TraitId, Traits, God(..))
import Traits.Decode as Decode
import Traits.EncodeElm as EncodeElm

import Dict exposing (Dict)
import Elm.CodeGen as Elm
import Elm.Pretty
import Json.Decode as Decode

main : Program () Model Msg
main = Platform.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =
  { traits : Traits
  , texts : Dict String String
  , filesWritten : List String
  }

type Msg
  = Exit
  | ConsoleEvent (Result Decode.Error Console.Event)

type File
  = TraitsJson Traits
  | Dxf God Layout
  | TextsJson (Dict String String)

init : () -> (Model, Cmd Msg)
init _ =
  ( { traits = Traits.empty
    , texts = Dict.empty
    , filesWritten = []
    }
  , Cmd.batch
    [ Console.write "start"
    , Console.readFile "traits.json"
    , Console.readFile "en.json"
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
          { model
          | traits = Traits.addLayout god layout model.traits
          }
            |> checkDone
        Ok (TextsJson texts) -> 
          let output = (generateTexts texts) in
          ( {model | texts = texts}
          , Console.writeFile "Texts/Generated.elm" output
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
    ConsoleEvent (Ok (Console.WriteFile name (Ok _))) ->
      let written = name :: model.filesWritten in
      ( { model | filesWritten = written }
      , if List.length written == 2 then
          Cmd.batch
            [ Console.exit
            , Console.write "done"
            ]
        else
          Cmd.none
      )
    ConsoleEvent (Ok (Console.WriteFile name (Err err))) ->
      (model, Console.write ("Failed to write " ++ name ++ " : " ++ err))
    ConsoleEvent (Err err) ->
      (model, Console.write ("event decode failed " ++ (Decode.errorToString err)))

checkDone : Model -> (Model, Cmd Msg)
checkDone model =
  let
    allLayouts = model.traits
      |> Traits.allGods
      |> List.all (Traits.dataLayout >> Layout.isEmpty >> not)
  in
    if allLayouts then
      let contents = (generateFile model.traits) in
      ( model
      , Cmd.batch
        [ Console.write "load complete"
        , Console.writeFile "Traits/Generated.elm" contents
        ]
      )
    else
      (model, Cmd.none)

generateFile : Traits -> String
generateFile traits =
  Elm.file
    (Elm.normalModule
      ["Traits", "Generated"]
      []
    )
    [ Elm.importStmt
      ["Traits"]
      Nothing
      (Just Elm.exposeAll)
    , Elm.importStmt
      ["Layout"]
      Nothing
      (Just Elm.exposeAll)
    , Elm.importStmt
      ["Color"]
      Nothing
      Nothing
    , Elm.importStmt
      ["Set"]
      Nothing
      (Just (Elm.exposeExplicit [Elm.closedTypeExpose "Set"]))
    ]
    (EncodeElm.traits traits)
    Nothing
    |> Elm.Pretty.pretty 80

generateTexts : Dict String String -> String
generateTexts texts =
  Elm.file
    (Elm.normalModule
      ["Texts", "Generated"]
      []
    )
    [ Elm.importStmt
      ["Dict"]
      Nothing
      (Just (Elm.exposeExplicit [Elm.closedTypeExpose "Dict"]))
    ]
    (EncodeElm.texts texts)
    Nothing
    |> Elm.Pretty.pretty 80

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
    "en.json" ->
      Decode.decodeString Decode.texts contents
        |> Result.map TextsJson
        |> Result.mapError Decode.errorToString
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
