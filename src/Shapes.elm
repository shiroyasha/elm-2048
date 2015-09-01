module Shapes where

import Color exposing (..)
import Graphics.Collage exposing (..)

roundedSquare : Int -> Int -> Color -> Form
roundedSquare size radius color =
  let
    size'   = toFloat size
    radius' = toFloat radius

    innerSquareSize = size' - radius' * 2

    circlePositions = [ ( innerSquareSize/2,  innerSquareSize/2)
                      , (-innerSquareSize/2,  innerSquareSize/2)
                      , ( innerSquareSize/2, -innerSquareSize/2)
                      , (-innerSquareSize/2, -innerSquareSize/2)
                      ]

    borderPositions = [ (0, innerSquareSize/2)
                      , (0, -innerSquareSize/2)
                      , ( innerSquareSize/2, 0)
                      , (-innerSquareSize/2, 0)
                      ]

    circleShapes = List.repeat 4 (circle radius')
    borderShapes = [ rect innerSquareSize (radius' * 2)
                   , rect innerSquareSize (radius' * 2)
                   , rect (radius' * 2) innerSquareSize
                   , rect (radius' * 2) innerSquareSize
                   ]

    circleForms = List.map (filled color) circleShapes
    borderForms = List.map (filled color) borderShapes

    innerSquare = square innerSquareSize |> filled color

    circles     = List.map2 move circlePositions circleForms
    borders     = List.map2 move borderPositions borderForms

  in
     group <| innerSquare :: List.concat [circles, borders]
