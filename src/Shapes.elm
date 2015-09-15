module Shapes where

import Text
import Color exposing (..)
import Graphics.Collage exposing (..)
import Units exposing (..)
import Graphics.Collage as Collage exposing (..)


gridBackground : Size -> Form
gridBackground size = roundedRect size 3 (Color.rgb 187 173 160)


cellBase : Float -> Form
cellBase size = roundedSquare size 3 (backgroungColor 0)


cell : Float -> Int -> Form
cell size number =
  let
      bg = roundedSquare size 3 (backgroungColor number)

      fg = label (labelSize size number) number
  in
     group [bg, fg]


-- Private

backgroungColor : Int -> Color.Color
backgroungColor number =
  case number of
    0 -> Color.rgb 204 192 179
    2 -> Color.rgb 238 228 218
    4 -> Color.rgb 236 224 200
    8 -> Color.rgb 241 176 120
    16 -> Color.rgb 235 140 82
    32 -> Color.rgb 243 123 96
    64 -> Color.rgb 233 89 55
    128 -> Color.rgb 242 216 106
    256 -> Color.rgb 231 191 41
    512 -> Color.rgb 231 191 41
    1024 -> Color.rgb 228 183 19
    2048 -> Color.rgb 238 195 3


textColor : Int -> Color.Color
textColor number =
  case number of
    2 -> Color.rgb 120 110 101
    4 -> Color.rgb 120 110 101
    _ -> Color.white


label : Float -> Int -> Form
label size number =
   (if number > 0 then toString number else "")
   |> Text.fromString
   |> Text.color (textColor number)
   |> Text.height size
   |> Text.bold
   |> text
   |> Collage.move (0, 7)


labelSize : Float -> Int -> Float
labelSize cellSize number =
  if | number < 100 -> cellSize / 2
     | number > 100 && number < 1000 -> cellSize / 2.5
     | number > 1000  -> cellSize / 3


roundedSquare : Float -> Float -> Color -> Form
roundedSquare size radius color = roundedRect (size, size) radius color


roundedRect : Size -> Float -> Color -> Form
roundedRect (width, height) radius color =
  let
    innerWidth  = width - radius * 2
    innerHeight = height - radius * 2

    circlePositions = [ ( innerWidth/2,  innerHeight/2)
                      , (-innerWidth/2,  innerHeight/2)
                      , ( innerWidth/2, -innerHeight/2)
                      , (-innerWidth/2, -innerHeight/2)
                      ]

    borderPositions = [ (0, innerHeight/2)
                      , (0, -innerHeight/2)
                      , ( innerWidth/2, 0)
                      , (-innerWidth/2, 0)
                      ]

    circleShapes = List.repeat 4 (circle radius)
    borderShapes = [ rect innerWidth (radius * 2)
                   , rect innerWidth (radius * 2)
                   , rect (radius * 2) innerHeight
                   , rect (radius * 2) innerHeight
                   ]

    circleForms = List.map (filled color) circleShapes
    borderForms = List.map (filled color) borderShapes

    innerRect = rect innerWidth innerHeight |> filled color

    circles     = List.map2 move circlePositions circleForms
    borders     = List.map2 move borderPositions borderForms

  in
     group <| innerRect :: List.concat [circles, borders]
