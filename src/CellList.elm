module CellList (squash) where

import Cell

squash : List Cell.Model -> List Cell.Model
squash cells =
  let
    groupedCells = cells |> groupWithZeroes |> groupPairs

    moveCellsToCell cells cell =
      let
        movement = Cell.Move cell.matrixPosition cell.position
      in
        List.map (Cell.update movement) cells
  in
    List.map2 moveCellsToCell groupedCells cells |> List.concat


-- Private

groupPairs: List (List Cell.Model) -> List (List Cell.Model)
groupPairs list =
  case list of
    [] -> []
    [x] -> [x]
    x::y::xs -> if (x |> List.map .number |> List.sum) == (y |> List.map .number |> List.sum)
                   then (List.concat [x, y]) :: (groupPairs xs)
                   else x :: (groupPairs (y::xs))

groupWithZeroes: List Cell.Model -> List (List Cell.Model)
groupWithZeroes list =
  case list of
    [] -> []
    _ -> (takeWithZeroes list) :: (groupWithZeroes (dropWithZeroes list))


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
