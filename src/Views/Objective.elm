module Views.Objective where

import Html exposing (..)

render = toElement 370 50 <| p []
  [ span [] [text "Join the numbers and get to the"]
  , strong [] [text " 2048 tile!"]
  ]
