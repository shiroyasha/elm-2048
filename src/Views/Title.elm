module Views.Title where

import Html exposing (..)

render = toElement 370 100 <| h1 [] [text "2048"]
