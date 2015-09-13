module Grid where

import Cell
import Matrix
import Shapes
import Color

import Graphics.Collage exposing (..)

type alias Model =
  { cells: Matrix.Matrix Cell.Model
  , size: (Float, Float)
  }

cellPaddingRatio = 0.95

cellPadding : (Float, Float) -> (Int, Int) -> Float
cellPadding (width, height) (rows, cols) =
  let
      factor = (rows+1|> toFloat) + (cols |> toFloat) * (cellPaddingRatio / (1 - cellPaddingRatio))
  in
    width / (2 * factor)


cellSize : (Float, Float) -> (Int, Int) -> Float
cellSize (width, height) (rows, cols) =
  (cellPaddingRatio / (1 - cellPaddingRatio)) * 2 * (cellPadding (width, height) (rows, cols))


cellPosition : (Float, Float) -> (Int, Int) -> (Int, Int) -> (Float, Float)
cellPosition (width, height) (rows, cols) (x, y) =
  let
      padding' = cellPadding (width, height) (rows, cols)
      cellSize' = cellSize (width, height) (rows, cols)

      cellSizeWithPadding' = 2*padding' + cellSize'

      x' = padding' + (cellSizeWithPadding' * (x |> toFloat)) - width/2 + cellSizeWithPadding'/2
      y' = padding' + (cellSizeWithPadding' * (y |> toFloat)) - height/2 + cellSizeWithPadding'/2
  in
     (x', y')


init : (Float, Float) -> (Int, Int) -> Model
init size (rows, cols) =
  let
    cellSize' = (cellSize size (rows, cols)) * 0.95

    cell x y = Cell.init (cellPosition size (rows, cols) (x, y)) cellSize' 2
  in
    { cells = Matrix.matrix rows cols cell, size = size }


-- UPDATE

type Action = Tick Float

update : Action -> Model -> Model
update action model =
  case action of
    Tick dt ->
      let
          cells' = Matrix.map (Cell.update (Cell.Tick dt)) model.cells
      in
        { model | cells <- cells' }


-- VIEW

view model =
  let
      bgColor = Color.rgb 187 173 160
      bgWidth = fst model.size |> round
      bgHeight = snd model.size |> round

      bg = Shapes.roundedRect bgWidth bgHeight 3 bgColor

      cells = group (Matrix.map Cell.view model.cells |> Matrix.flatten)
  in
     group [bg, cells]
