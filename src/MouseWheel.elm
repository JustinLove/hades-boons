module MouseWheel exposing (onWheel, wheelDecoder)

import Html
import Html.Events
import Json.Decode as Decode

type alias Point = (Float, Float)

onWheel : (Point -> Int -> msg) -> Html.Attribute msg
onWheel tagger =
  Html.Events.stopPropagationOn "wheel" (wheelDecoder tagger)

wheelDecoder : (Point -> Int -> msg) -> Decode.Decoder ( msg, Bool )
wheelDecoder tagger =
  Decode.map (\v -> (v, True))
    (Decode.map2 tagger
      clientDecoder
      (Decode.field "deltaY" Decode.int)
    )

clientDecoder : Decode.Decoder Point
clientDecoder =
  Decode.map2 Tuple.pair
    (Decode.field "clientX" Decode.float)
    (Decode.field "clientY" Decode.float)

