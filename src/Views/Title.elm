module Views.Title where

import Text exposing (..)
import Color exposing (..)
import Shapes exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

render = collage 330 100
  ["2048"
  |> Text.fromString
  |> Text.color (rgb 120 110 101)
  |> Text.height 60
  |> Text.bold
  |> text
  |> move (-80, 7)]
