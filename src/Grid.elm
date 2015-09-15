module Grid where

import Cell
import Matrix
import Shapes
import Color
import MatrixLayout
import Random

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
    , cellsToAdd = 0
    , seed = seed
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

emptyCells model = model.cells |> Matrix.flatten |> List.filter (\cell -> cell.number == 0)


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


-- Grouping

takeWithZeroes: List Cell.Model -> List Cell.Model
takeWithZeroes list =
  case list of
    [] -> []
    x::xs -> if x.number == 0
                then x :: (takeWithZeroes xs)
                else [x]

dropWithZeroes: List Cell.Model -> List Cell.Model
dropWithZeroes list =
  case list of
    [] -> []
    x::xs -> if x.number == 0
                then dropWithZeroes xs
                else xs


groupWithZeroes: List Cell.Model -> List (List Cell.Model)
groupWithZeroes list =
  case list of
    [] -> []
    _ -> (takeWithZeroes list) :: (groupWithZeroes (dropWithZeroes list))


groupPairs: List (List Cell.Model) -> List (List Cell.Model)
groupPairs list =
  case list of
    [] -> []
    [x] -> [x]
    x::y::xs -> if (x |> List.map .number |> List.sum) == (y |> List.map .number |> List.sum)
                   then (List.concat [x, y]) :: (groupPairs xs)
                   else x :: (groupPairs (y::xs))

-- Grouping


moveCellsToCell : List Cell.Model -> Cell.Model -> List Cell.Model
moveCellsToCell cells cell =
  let
    movement = Cell.Move cell.matrixPosition cell.position
  in
    List.map (Cell.update movement) cells


moveList : List Cell.Model -> List Cell.Model
moveList cells =
  List.map2 moveCellsToCell (cells |> groupWithZeroes |> groupPairs) cells |> List.concat


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

view : Model -> Form
view model =
  let
      bg = Shapes.roundedRect (MatrixLayout.gridSize model.layout) 3 (Color.rgb 187 173 160)

      bases = model.cells |> Matrix.flatten |> List.map Cell.viewBase |> group
      cells = model.cells |> Matrix.flatten |> List.map Cell.view |> group
  in
     group [bg, bases, cells]
