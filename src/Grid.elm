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

    cell x y =
      let
          matrixPosition = (x, y)
          position = MatrixLayout.cellPosition layout matrixPosition
      in
          Cell.init position matrixPosition layout.cellSize 0

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
    cell = Cell.init position matrixPosition model.layout.cellSize number
  in
    { model | cells <- Matrix.set x y cell model.cells }



takeMergable : List Cell.Model -> List Cell.Model
takeMergable list =
  case list of
    [] -> []
    [x] -> [x]
    x::y::xs -> if | x.number == 0        -> x :: takeMergable (y::xs)
                   | x.number == y.number -> [x, y]
                   | otherwise -> [x]


dropMergable : List Cell.Model -> List Cell.Model
dropMergable list =
  case list of
    [] -> []
    [x] -> []
    x::y::xs -> if | x.number == 0        -> dropMergable (y::xs)
                   | x.number == y.number -> xs
                   | otherwise            -> y::xs


groupCells : List Cell.Model -> List (List Cell.Model)
groupCells list =
  case list of
   [] -> []
   x::xs -> (takeMergable (x::xs)) :: (groupCells (dropMergable (x::xs)))


moveCellsToCell : List Cell.Model -> Cell.Model -> List Cell.Model
moveCellsToCell cells cell =
  let
    movement = Cell.Move cell.matrixPosition cell.position
  in
    List.map (Cell.update movement) cells


moveList : List Cell.Model -> List Cell.Model
moveList cells =
  List.map2 moveCellsToCell (groupCells cells) cells |> List.concat


move dir model =
  case dir of
    Left ->
      { model | cells <- model.cells |> Matrix.toList |> List.map moveList |> Matrix.fromList }
    Right ->
      { model | cells <- model.cells |> Matrix.toList |> List.map List.reverse |> List.map moveList |> List.map List.reverse |> Matrix.fromList }
    Up ->
      { model | cells <- model.cells |> Matrix.transpose |> Matrix.toList |> List.map moveList |> Matrix.fromList |> Matrix.transpose }
    Down ->
      { model | cells <- model.cells |> Matrix.transpose |> Matrix.toList |> List.map List.reverse |> List.map moveList |> List.map List.reverse |> Matrix.fromList |> Matrix.transpose }
    _ ->
      model


mergeOne : Cell.Model -> Model -> Model
mergeOne cell model =
  case cell.state of
    Cell.WaitingForMerge toPosition ->
      let
        xNew = fst toPosition
        yNew = snd toPosition

        xOld = fst cell.matrixPosition
        yOld = snd cell.matrixPosition

        cells' = model.cells
          |> Matrix.update xNew yNew (Cell.update (Cell.Add cell.number))
          |> Matrix.update xOld yOld (Cell.update (Cell.Substract cell.number))
      in
        { model | cells <- cells' }
    _ ->
      model

mergeAll : Model -> List Cell.Model -> Model
mergeAll model cells =
  case cells of
    [] -> model
    cell::xs -> mergeOne cell (mergeAll model xs)


merge : Model -> Model
merge model =
  mergeAll model (model.cells |> Matrix.flatten)


isGridStationary model =
  model.cells |> Matrix.flatten |> List.all (isStationaty)


isStationaty cell =
  cell.state == Cell.Stationary

isNotStationaty cell =
  cell.state /= Cell.Stationary


update : Action -> Model -> Model
update action model =
  case action of
    Tick dt ->
      { model | cells <- Matrix.map (Cell.update (Cell.Tick dt)) model.cells } |> merge

    NewCell (position, number) ->
      addCell position number model

    Move dir ->
      if isGridStationary model
         then move dir model
         else model


-- VIEW

view : Model -> Form
view model =
  let
      bg = Shapes.roundedRect (MatrixLayout.gridSize model.layout) 3 (Color.rgb 187 173 160)

      bases = model.cells |> Matrix.flatten |> List.map Cell.viewBase |> group
      cells = model.cells |> Matrix.flatten |> List.map Cell.view |> group
  in
     group [bg, bases, cells]
