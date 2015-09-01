module Views.Grid where

import Matrix exposing (Matrix)
import Text exposing (..)
import Color exposing (..)
import Shapes
import Views.Cell
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Config exposing (Config)


cellPosition : Config -> Matrix.Location -> Int -> (Float, Float)
cellPosition config location _ =
  let
     gridSize = (config.cell.size + 2 * config.cell.padding) * config.grid.rows |> toFloat

     sizeWithPadding = toFloat (config.cell.size + 2 * config.cell.padding)

     x = ((toFloat <| Matrix.row location) + 1/2) * sizeWithPadding
     y = ((toFloat <| Matrix.col location) + 1/2) * sizeWithPadding
  in
     (x - gridSize/2, y - gridSize/2)


cellPositions : Config -> Matrix Int -> List (Float, Float)
cellPositions config model = Matrix.mapWithLocation (cellPosition config) model |> Matrix.flatten


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
