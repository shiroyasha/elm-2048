module Models.Grid where

import Random exposing (Seed)

import Matrix exposing (Matrix)
import Models.GridRow

type alias Points = Int
type alias Grid = Matrix Int

type Action = SquashUp | SquashDown | SquashLeft | SquashRight | NoAction


emptyGrid : Int -> Int -> Grid
emptyGrid width height = Matrix.repeat width height 0


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


numbers : Grid -> List Int
numbers = Matrix.toList >> List.concat


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



nth: Int -> List a -> Maybe a
nth index list =
  case index of
    0 -> List.head list
    _ -> nth (index - 1) (Maybe.withDefault [] <| List.tail list)


addRandomCell: Seed -> Grid -> (Seed, Grid)
addRandomCell seed grid =
  let
    (randomNumber, seed') = Random.generate (Random.int 0 100) seed

    positions = emptyPositions grid

    randomPosition = nth (randomNumber % (List.length positions)) positions

    grid' = case randomPosition of
      Just position -> addCell position grid
  in
     (seed', grid')


squash: Grid -> (Points, Grid)
squash grid =
  let
      (points, rows') = grid |> Matrix.toList |> List.map Models.GridRow.squash |> List.unzip
  in
     (points |> List.sum, rows' |> Matrix.fromList)
