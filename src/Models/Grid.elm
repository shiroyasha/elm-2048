module Models.Grid where

import Matrix exposing (Matrix)

type alias Grid = Matrix Int


type Action = SquashUp | SquashDown | SquashLeft | SquashRight | NoAction


grid : Int -> Int -> Grid
grid width height = Matrix.repeat width height 0 |> Matrix.set 1 1 2


update : Action -> Grid -> Grid
update action = case action of
  SquashLeft  -> Matrix.toList >> List.map squashRowLeft >> Matrix.fromList
  SquashRight -> Matrix.toList >> List.map squashRowRight >> Matrix.fromList
  SquashUp    -> Matrix.transpose >> update SquashLeft >> Matrix.transpose
  SquashDown  -> Matrix.transpose >> update SquashRight >> Matrix.transpose
  NoAction    -> identity


emptyPositions : Grid -> List (Int, Int)
emptyPositions =
  Matrix.toIndexedList
  >> List.filter (\(_, _, number) -> number == 0)
  >> List.map (\(x, y, _) -> (x, y))


sumTheSame list =
  let
     firstTwoTheSame list = List.take 1 list == (list |> List.take 2 |> List.drop 1)
  in
     if | List.length list == 0 -> list
        | List.length list == 1 -> list
        | List.length list >= 2 && firstTwoTheSame list ->
            sumTheSame ((list |> List.take 2 |> List.sum) :: (List.drop 2 list))
        | otherwise ->
            (list |> List.take 1 |> List.sum) :: (sumTheSame (List.drop 1 list))


squashRowLeft : List Int -> List Int
squashRowLeft list =
  let
      numbers = List.filter (\x -> x /= 0) list |> sumTheSame

      numberOfZeroes = (List.length list) - (List.length numbers)
  in
      List.concat [numbers, List.repeat numberOfZeroes 0]


squashRowRight : List Int -> List Int
squashRowRight = List.reverse >> squashRowLeft >> List.reverse
