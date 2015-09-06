module Views.Cell where

import Text exposing (..)
import Color exposing (..)
import Matrix exposing (Matrix)
import Graphics.Collage exposing (..)
import Shapes

backgroungColor : Int -> Color
backgroungColor number =
  case number of
    0 -> rgb 204 192 179
    2 -> rgb 238 228 218
    4 -> rgb 236 224 200
    8 -> rgb 241 176 120
    16 -> rgb 235 140 82
    32 -> rgb 243 123 96
    64 -> rgb 233 89 55
    128 -> rgb 242 216 106
    256 -> rgb 231 191 41
    512 -> rgb 231 191 41
    1024 -> rgb 228 183 19
    2048 -> rgb 238 195 3
    _ -> gray


textColor : Int -> Color
textColor number =
  case number of
    2 -> rgb 120 110 101
    4 -> rgb 120 110 101
    _ -> white


label : Float -> Int -> Form
label size number =
   (if number > 0 then toString number else "")
   |> Text.fromString
   |> Text.color (textColor number)
   |> Text.height size
   |> Text.bold
   |> text
   |> move (0, 7)


labelSize : Int -> Int -> Float
labelSize cellSize number =
  if | number < 100 -> (toFloat cellSize) / 2
     | number > 100 && number < 1000 -> (toFloat cellSize) / 2.5
     | number > 1000  -> (toFloat cellSize) / 3


backgroung : Int -> Int -> Int -> Form
backgroung size radius number =
  Shapes.roundedSquare size radius (backgroungColor number)


cell : Int -> Int -> Int -> Form
cell size radius number =
  group [ backgroung size radius number, label (labelSize size number) number ]
