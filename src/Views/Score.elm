module Views.Score where

import Text exposing (..)
import Color exposing (..)
import Shapes exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

backgroung w h = Shapes.roundedRect (w - 15) (h - 40) 3 (rgb 187 173 160)


title =
  "SCORE"
  |> Text.fromString
  |> Text.color (rgb 238 228 218)
  |> Text.height 14
  |> Text.bold
  |> text
  |> move (0, 15)


label score =
  score
  |> toString
  |> Text.fromString
  |> Text.color white
  |> Text.height 20
  |> Text.bold
  |> text
  |> move (0, -5)


render: Int -> Element
render score =
  let
      width = 120
      height = 100
  in
     collage width height [group [backgroung width height, title, label score] |> move (0, -5)]
