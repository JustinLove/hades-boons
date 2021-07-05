module Dxf.Decode exposing (..)

import Dxf exposing (..)
import Dxf.Parser as Parser exposing (CodePair)

import Parser.Advanced

type Error
  = MismatchedType String
  | CodeNotFound Int
  | Failure String
  | WrongEntityType EntityType EntityType
  | NotAnEntity
  | ParseError (List (Parser.Advanced.DeadEnd Parser.Context Parser.Problem))
  | NoneOf

errorToString : Error -> String
errorToString err =
  case err of
    MismatchedType t -> "Mismatched type " ++ t
    CodeNotFound c -> "Code " ++ (String.fromInt c) ++ " not found"
    Failure reason -> reason
    WrongEntityType actual expected -> "Wrong entity type encountered"
    NotAnEntity -> "Was expecting an entity value and got something else"
    ParseError parseError -> Parser.deadEndsToString parseError
    NoneOf -> "No option to oneOf succeeded"

type alias Decoder a = List CodePair -> Result Error a

x : Value -> Result Error Float
x v =
  case v of
    X ex -> Ok ex
    _ -> Err (MismatchedType "x")

y : Value -> Result Error Float
y v =
  case v of
    Y why -> Ok why
    _ -> Err (MismatchedType "y")

intValue : Value -> Result Error Int
intValue v =
  case v of
    Integer i -> Ok i
    _ -> Err (MismatchedType "int value")

floatValue : Value -> Result Error Float
floatValue v =
  case v of
    FloatValue f -> Ok f
    _ -> Err (MismatchedType "float value")

angle : Value -> Result Error Float
angle v =
  case v of
    Angle a -> Ok a
    _ -> Err (MismatchedType "angle")

text : Value -> Result Error String
text v =
  case v of
    Text t -> Ok t
    _ -> Err (MismatchedType "text")

primaryText : Value -> Result Error String
primaryText v =
  case v of
    PrimaryText t -> Ok t
    _ -> Err (MismatchedType "primary text")

layer : Value -> Result Error String
layer v =
  case v of
    Layer l -> Ok l
    _ -> Err (MismatchedType "layer")

tag : GroupCode -> (Value -> Result Error a) -> Decoder a
tag c decoder list =
  case list of
    (group, v) :: rest ->
      if group == c then
        decoder v
      else if group == 0 then
        Err (CodeNotFound c)
      else
        tag c decoder rest
    [] ->
      Err (CodeNotFound c)

succeed : a -> Decoder a
succeed result _ = Ok result

fail : String -> Decoder a
fail error _ = Err (Failure error)

map : (a -> b) -> Decoder a -> Decoder b
map f decoder list =
  decoder list
    |> Result.map f

map2 : (a -> b -> c) -> Decoder a -> Decoder b-> Decoder c
map2 f decoder1 decoder2 list =
  Result.map2 f
    (decoder1 list)
    (decoder2 list)

andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f decoder list =
  case decoder list of
    Ok v -> (f v) list
    Err e -> Err e

maybe : Decoder a -> Decoder (Maybe a)
maybe decoder list =
  case decoder list of
    Ok v -> Ok (Just v)
    Err e -> Ok Nothing

oneOf : List (Decoder a) -> Decoder a
oneOf decoders list =
  case decoders of
    first :: rest ->
      case first list of
        Ok v -> Ok v
        Err _ -> oneOf rest list
    _ ->
      Err NoneOf

with : Decoder a -> Decoder (a -> b)-> Decoder b
with = map2 (|>)

withTag : GroupCode -> (Value -> Result Error a) -> Decoder (a -> b) -> Decoder b
withTag c decoder pipeline list =
  Result.map2 (|>)
    (tag c decoder list)
    (pipeline list)

every : GroupCode -> Decoder a -> Decoder (List a)
every targetCode decoder list =
  case list of
    (0, EntityType SectionEnd) :: rest ->
      Ok []
    (0, EntityType _) :: rest ->
      everyStep targetCode decoder [] rest
    [] ->
      Ok []
    _ ->
      everyStep targetCode decoder [] list

everyStep : GroupCode -> Decoder a -> List a -> Decoder (List a)
everyStep targetCode decoder reversedResults list =
  case list of
    (0, EntityType SectionEnd) :: rest ->
      Ok (List.reverse reversedResults)
    (0, EntityType _) :: rest ->
      Ok (List.reverse reversedResults)
    (code, _) :: rest ->
      if code == targetCode then
        case decoder list of
          Ok v -> everyStep targetCode decoder (v :: reversedResults) rest
          Err err -> Err err
      else
        everyStep targetCode decoder reversedResults rest
    [] ->
      Ok (List.reverse reversedResults)

entity : EntityType -> Decoder a -> Decoder a
entity kind decoder list =
  case list of
    (0, EntityType t) :: rest ->
      if kind == t then
        decoder rest
      else
        Err (WrongEntityType t kind)
    _ :: rest ->
      Err NotAnEntity
    [] ->
      Err NotAnEntity

entities : EntityType -> Decoder a -> Decoder (List a)
entities kind decoder list =
  list
    |> advanceToSection "ENTITIES"
    |> entitiesStep kind decoder []

entitiesStep : EntityType -> Decoder a -> List a -> Decoder (List a)
entitiesStep kind decoder reversedResults list =
  case list of
    (0, EntityType SectionEnd) :: rest ->
      Ok (List.reverse reversedResults)
    (0, EntityType t) :: rest ->
      if kind == t then
        (decoder
          |> andThen (\v -> entitiesStep kind decoder (v :: reversedResults))
        ) rest
      else
        entitiesStep kind decoder reversedResults rest
    _ :: rest ->
      entitiesStep kind decoder reversedResults rest
    [] ->
      Ok (List.reverse reversedResults)

advanceToSection : String -> List CodePair -> List CodePair
advanceToSection name list =
  case list of
    (0, EntityType SectionStart) :: (2, Name n) :: rest ->
      if n == name then
        rest
      else
        advanceToSection name rest
    _ :: rest ->
      advanceToSection name rest
    [] ->
      []

decodeString : Decoder a -> String -> Result Error a
decodeString decoder s =
  Parser.Advanced.run Parser.toList s
    |> Result.mapError ParseError
    |> Result.andThen (\list -> decoder list)
