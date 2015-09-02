module Views.Grid where

import Matrix exposing (Matrix)
import Text exposing (..)
import Color exposing (..)
import Shapes
import Views.Cell
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

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


cells : Config -> Matrix Int -> List Form
cells config model =
  model
  |> Matrix.map (Views.Cell.cell config.cell.size config.cell.radius)
  |> Matrix.flatten
  |> List.map2 move (cellPositions config model)


render : Config -> Matrix Int -> Element
render config model =
  let
      backgroungRadius = config.grid.radius
      backgroundSize   = (config.cell.size + 2 * config.cell.padding) * config.grid.rows + config.grid.padding
      backgroungColor  = rgb 187 173 160

      backgroung = Shapes.roundedSquare backgroundSize backgroungRadius backgroungColor
  in
     collage backgroundSize backgroundSize (backgroung :: cells config model)
