module Grid where

import Cell
import Matrix
import Shapes
import Color
import MatrixLayout

import Graphics.Collage exposing (..)
import Units exposing (..)

type alias Model =
  { cells: Matrix.Matrix Cell.Model
  , layout : MatrixLayout.Model
  }


init : Size -> MatrixSize -> Model
init size ((rows, cols) as matrixSize) =
  let
    layout = MatrixLayout.init size matrixSize

    cell x y = Cell.init (MatrixLayout.cellPosition layout (x, y)) layout.cellSize 0
  in
    { cells = Matrix.matrix rows cols cell
    , layout = layout
    }


-- UPDATE

type Action = Tick Float | NewCell (MatrixPosition, Int) | Move Direction


addCell : MatrixPosition -> Int -> Model -> Model
addCell ((x, y) as matrixPosition) number model =
  let
    position = MatrixLayout.cellPosition model.layout matrixPosition
    cell = Cell.init position model.layout.cellSize number
  in
    { model | cells <- Matrix.set x y cell model.cells }


update : Action -> Model -> Model
update action model =
  case action of
    Tick dt ->
      let
          cells' = Matrix.map (Cell.update (Cell.Tick dt)) model.cells
      in
        { model | cells <- cells' }

    NewCell (position, number) ->
      addCell position number model

    Move dir ->
      case dir of
        Left ->
          let
              cells = Matrix.indexedMap (\x y cell ->
                if cell.number == 0 then cell else Cell.update (Cell.Move (MatrixLayout.cellPosition model.layout (0, y))) cell) model.cells
          in
            { model | cells <- cells }
        Up ->
          model

-- VIEW

view : Model -> Form
view model =
  let
      bg = Shapes.roundedRect (MatrixLayout.gridSize model.layout) 3 (Color.rgb 187 173 160)

      cells = group (Matrix.map Cell.view model.cells |> Matrix.flatten)
  in
     group [bg, cells]
