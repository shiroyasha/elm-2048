module Models.Grid where

import Matrix exposing (Matrix)
import Models.GridRow

type alias Points = Int
type alias Grid = Matrix Int

type Action = SquashUp | SquashDown | SquashLeft | SquashRight | NoAction


emptyGrid : Int -> Int -> Grid
emptyGrid width height = Matrix.repeat width height 0


grid : Int -> Int -> Grid
grid width height = emptyGrid width height |> addCell (1, 1)


addCell : (Int, Int) -> Grid -> Grid
addCell (x, y) grid = Matrix.set x y 2 grid


emptyPositions : Grid -> List (Int, Int)
emptyPositions =
  Matrix.toIndexedList
  >> List.filter (\(_, _, number) -> number == 0)
  >> List.map (\(x, y, _) -> (x, y))


transpose: Grid -> Grid
transpose = Matrix.transpose


flip: Grid -> Grid
flip = Matrix.toList >> List.map List.reverse >> Matrix.fromList



update : Action -> Grid -> (Points, Grid)
update action grid =
  let
    (points, grid') = squash <| (grid |> case action of
      SquashLeft  -> identity
      SquashRight -> flip
      SquashUp    -> transpose
      SquashDown  -> transpose >> flip)

    grid'' = grid' |> case action of
      SquashLeft  -> identity
      SquashRight -> flip
      SquashUp    -> transpose
      SquashDown  -> flip >> transpose
  in
     (points, grid'')


squash: Grid -> (Points, Grid)
squash grid =
  let
      (points, rows') = grid |> Matrix.toList |> List.map Models.GridRow.squash |> List.unzip
  in
     (points |> List.sum, rows' |> Matrix.fromList)
