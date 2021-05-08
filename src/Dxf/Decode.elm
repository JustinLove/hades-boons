module Dxf.Decode exposing (..)

import Dxf.Parser as Parser exposing
  ( Value(..)
  , GroupCode
  , CodePair
  )

type Error
  = MismatchedType String
  | CodeNotFound Int
  | Failure String

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

text : Value -> Result Error String
text v =
  case v of
    Text t -> Ok t
    _ -> Err (MismatchedType "text")

tag : GroupCode -> (Value -> Result Error a) -> List CodePair -> Result Error a
tag c decoder list =
  case list of
    (group, v) :: rest ->
      if group == c then
        decoder v
      else
        tag c decoder rest
    [] ->
      Err (CodeNotFound c)

succeed : a -> List CodePair -> Result Error a
succeed result _ = Ok result

fail : String -> List CodePair -> Result Error a
fail error _ = Err (Failure error)

map : (a -> b) -> (List CodePair -> Result Error a) -> List CodePair -> Result Error b
map f decoder list =
  decoder list
    |> Result.map f

map2 : (a -> b -> c) -> (List CodePair -> Result Error a) -> (List CodePair -> Result Error b)-> List CodePair -> Result Error c
map2 f decoder1 decoder2 list =
  Result.map2 f
    (decoder1 list)
    (decoder2 list)

andThen : (a -> (List CodePair -> Result Error b)) -> (List CodePair -> Result Error a) -> List CodePair -> Result Error b
andThen f decoder list =
  case decoder list of
    Ok v -> (f v) list
    Err e -> Err e

with : (List CodePair -> Result Error a) -> (List CodePair -> Result Error (a -> b))-> List CodePair -> Result Error b
with = map2 (|>)

withTag : GroupCode -> (Value -> Result Error a) -> (List CodePair -> Result Error (a -> b)) -> List CodePair -> Result Error b
withTag c decoder pipeline list =
  Result.map2 (|>)
    (tag c decoder list)
    (pipeline list)
