module Shapes where

import Color exposing (..)
import Graphics.Collage exposing (..)

roundedSquare : Int -> Int -> Color -> Form
roundedSquare size radius color = roundedRect size size radius color


roundedRect : Int -> Int -> Int -> Color -> Form
roundedRect width height radius color =
  let
    width'  = toFloat width
    height' = toFloat height
    radius' = toFloat radius

    innerWidth  = width' - radius' * 2
    innerHeight = height' - radius' * 2

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

    circleShapes = List.repeat 4 (circle radius')
    borderShapes = [ rect innerWidth (radius' * 2)
                   , rect innerWidth (radius' * 2)
                   , rect (radius' * 2) innerHeight
                   , rect (radius' * 2) innerHeight
                   ]

    circleForms = List.map (filled color) circleShapes
    borderForms = List.map (filled color) borderShapes

    innerRect = rect innerWidth innerHeight |> filled color

    circles     = List.map2 move circlePositions circleForms
    borders     = List.map2 move borderPositions borderForms

  in
     group <| innerRect :: List.concat [circles, borders]
