module Views.Cell where

import Text exposing (..)
import Color exposing (..)
import Matrix exposing (Matrix)
import Graphics.Collage exposing (..)
import Shapes

backgroungColor : Int -> Color
backgroungColor number =
  case number of
    0 -> white
    2 -> rgb 238 228 218
    4 -> rgb 119 110 101
    8 -> black
    16 -> black
    32 -> black
    64 -> black
    128 -> black
    256 -> black
    512 -> black
    1024 -> black
    2048 -> white
    _ -> gray


textColor : Int -> Color
textColor number =
  case number of
    0 -> white
    2 -> rgb 119 110 101
    4 -> rgb 119 110 101
    8 -> black
    16 -> black
    32 -> black
    64 -> black
    128 -> black
    256 -> black
    512 -> black
    1024 -> black
    2048 -> white
    _ -> gray


label : Float -> Int -> Form
label size number =
   (if number > 0 then toString number else "")
   |> Text.fromString
   |> Text.color (textColor number)
   |> Text.height size
   |> Text.bold
   |> Text.monospace
   |> text


backgroung : Int -> Int -> Int -> Form
backgroung size radius number =
  Shapes.roundedSquare size radius (backgroungColor number)


cell : Int -> Int -> Int -> Form
cell size radius number =
  group [ backgroung size radius number, label ((toFloat size) / 3) number ]
