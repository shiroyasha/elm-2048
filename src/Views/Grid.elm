module Views.Grid where

import Matrix exposing (Matrix)
import Text exposing (..)
import Color exposing (..)
import Shapes
import Views.Cell
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Models.GameState exposing (..)

import Config exposing (Config)


cellPosition : Config -> Int -> Int -> Int -> (Float, Float)
cellPosition config x y number =
  let
     gridSize = (config.cell.size + 2 * config.cell.padding) * config.grid.rows |> toFloat

     sizeWithPadding = toFloat (config.cell.size + 2 * config.cell.padding)

     x' = ((toFloat x) + 1/2) * sizeWithPadding - gridSize/2
     y' = ((toFloat y) + 1/2) * sizeWithPadding - gridSize/2
  in
     (x', y')


cellPositions : Config -> Matrix Int -> List (Float, Float)
cellPositions config model = Matrix.indexedMap (cellPosition config) model |> Matrix.flatten


cells : Config -> Matrix Int -> Form
cells config model =
  model
  |> Matrix.map (Views.Cell.cell config.cell.size config.cell.radius)
  |> Matrix.flatten
  |> List.map2 move (cellPositions config model)
  |> group


message: String -> Int -> Int -> Form
message label width height = group
  [ rect (toFloat width) (toFloat height) |> filled (rgba 255 255 255 0.3)
  , label |> Text.fromString |> Text.height 80 |> Text.bold |> Text.color (rgb 120 110 101) |> text
  ]

won: Int -> Int -> Form
won width height = message "You won!" width height


lost: Int -> Int -> Form
lost width height = message "You lost!" width height


render : Config -> GameState -> Element
render config {grid, phase} =
  let
      backgroungRadius = config.grid.radius
      backgroundSize   = (config.cell.size + 2 * config.cell.padding) * config.grid.rows + config.grid.padding
      backgroungColor  = rgb 187 173 160

      backgroung = Shapes.roundedSquare backgroundSize backgroungRadius backgroungColor

      forms = case phase of
        InProgress -> []
        GameOver -> [lost backgroundSize backgroundSize]
        Won -> [won backgroundSize backgroundSize]

      forms' = backgroung :: cells config grid :: forms
  in
     collage backgroundSize backgroundSize forms'
