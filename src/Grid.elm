module Grid where

import Cell
import Matrix
import Shapes
import Color
import MatrixLayout
import Random
import CellList

import Graphics.Collage exposing (..)
import Units exposing (..)

type alias Model =
  { cells: Matrix.Matrix Cell.Model
  , layout : MatrixLayout.Model
  , cellsToAdd: Int
  , seed: Random.Seed
  }


init : Size -> MatrixSize -> Random.Seed -> Model
init size ((rows, cols) as matrixSize) seed =
  { cells = Matrix.matrix rows cols (\x y -> Cell.init 0 (x, y))
  , layout = MatrixLayout.init size matrixSize
  , cellsToAdd = 0
  , seed = seed
  }


-- UPDATE

type Action = Tick Float | NewCell (MatrixPosition, Int) | Move Direction


addCell : MatrixPosition -> Int -> Model -> Model
addCell ((x, y) as matrixPosition) number model =
  let
    cell = Cell.init number matrixPosition
  in
    { model | cells <- Matrix.set x y cell model.cells }


emptyCells model = model.cells |> Matrix.flatten |> List.filter (\cell -> cell.number == 0)
numberedCells model = model.cells |> Matrix.flatten |> List.filter (\cell -> cell.number /= 0)


nth: Int -> List a -> Maybe a
nth index list =
  case index of
    0 -> List.head list
    _ -> nth (index - 1) (Maybe.withDefault [] <| List.tail list)


addCellToRandomPosition: Int -> Model -> Model
addCellToRandomPosition number model =
  let
      cells = emptyCells model

      (randomNumber, seed') = Random.generate (Random.int 0 100) model.seed

      randomCell = nth (randomNumber % (List.length cells)) cells

      model' = case randomCell of
        Just c -> addCell (c.matrixPosition) number model
        Nothing -> model'

  in
    { model' | cellsToAdd <- model'.cellsToAdd - 1, seed <- seed' }

move dir model =
  case dir of
    Left ->
      { model | cells <- model.cells |> Matrix.toList |> List.map CellList.squash |> Matrix.fromList }
    Right ->
      { model | cells <- model.cells |> Matrix.toList |> List.map List.reverse |> List.map CellList.squash |> List.map List.reverse |> Matrix.fromList }
    Up ->
      { model | cells <- model.cells |> Matrix.transpose |> Matrix.toList |> List.map CellList.squash |> Matrix.fromList |> Matrix.transpose }
    Down ->
      { model | cells <- model.cells |> Matrix.transpose |> Matrix.toList |> List.map List.reverse |> List.map CellList.squash |> List.map List.reverse |> Matrix.fromList |> Matrix.transpose }
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

scheduleCell model =
  { model | cellsToAdd <- model.cellsToAdd + 1 }


update : Action -> Model -> Model
update action model =
  case action of
    Tick dt ->
      let
        model' = { model | cells <- Matrix.map (Cell.update (Cell.Tick dt)) model.cells }
        model'' = merge model'

        model''' = if (isGridStationary model) && (model.cellsToAdd > 0)
                      then addCellToRandomPosition 2 model''
                      else model''
      in
         model'''

    NewCell (position, number) ->
      addCell position number model

    Move dir ->
      if isGridStationary model
         then model |> move dir |> if dir /= Nowhere then scheduleCell else identity
         else model


-- VIEW

viewCellBases : Model -> Form
viewCellBases model =
  let
    positions = model.layout |> MatrixLayout.cellPositions

    base position = Shapes.cellBase model.layout.cellSize |> Graphics.Collage.move position
  in
    group (List.map base positions)


viewBg : Model -> Form
viewBg model = Shapes.gridBackground (MatrixLayout.gridSize model.layout)


viewCells : Model -> Form
viewCells model =
  model |> numberedCells |> List.map (Cell.view model.layout) |> group


view : Model -> Form
view model =
  group [viewBg model, viewCellBases model, viewCells model]
