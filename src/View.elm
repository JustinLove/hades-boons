module View exposing (Msg(..), document, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Json.Decode

type Msg
  = None

css = """
body { margin: 0; overflow: hidden; }
#top { padding: 8px; }
h2 { text-align: center; margin: 0;}
.error-message { text-align: center; margin-top: 2em; }
footer { position: fixed; bottom: 0;}
svg.icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  vertical-align: -0.2em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}

#top.dark {
  background-color: #301c2b;
  color: #e5e3e8;
}
.dark .icon-github { color: #888; }
.dark .icon-twitter { color: #55acee; }
.dark .icon-twitch { color: #6441A4; }
.dark a:link, .dark a:visited { color: #6441a4; }
.dark a:hover, .dark a:active { color: rgb(218, 216, 222); }

#top.light {
  background-color: #fff;
  color: #232127;
}
.light .icon-github { color: #888; }
.light .icon-twitter { color: #55acee; }
.light .icon-twitch { color: #e2dbf0; }
.light a:link, .light a:visited { color: #e2dbf0; }
.light a:hover, .light a:active { color: rgb(218, 216, 222); }
"""

document tagger model =
  { title = "Schedule From Videos"
  , body = [Html.map tagger (view model)]
  }

view model = 
  div [ id "top", class "dark" ]
    [ node "style" [] [ text css ]
    , displayFooter
    ]

displayFooter : Html msg
displayFooter =
  footer []
    [ a
      [ href "https://github.com/JustinLove/schedule-from-videos"
      , target "_blank"
      , rel "noopener"
      ]
      [ icon "github", text "schedule-from-videos" ]
    , text " "
    , a
        [ href "https://twitter.com/wondible"
        , target "_blank"
        , rel "noopener"
        ]
        [ icon "twitter", text "@wondible" ]
    , text " "
    , a
      [ href "https://twitch.tv/wondible"
      , target "_blank"
      , rel "noopener"
      ]
      [ icon "twitch", text "wondible" ]
    ]

icon : String -> Html msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]
