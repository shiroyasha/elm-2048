import Matrix exposing (Matrix)

import Config
import Views.Grid
import Random
import Keyboard
import Time exposing (..)
import Debug


defaultGrid : Int -> Int -> Matrix Int
defaultGrid rows cols =
  Matrix.repeat rows cols 0
  |> Matrix.set 3 1 2
  |> Matrix.set 3 2 4
  |> Matrix.set 1 1 2
  |> Matrix.set 1 2 8


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


-- squoshing rows

squashRowLeft : List Int -> List Int
squashRowLeft list =
  let
      numbers = List.filter (\x -> x /= 0) list |> sumTheSame

      numberOfZeroes = (List.length list) - (List.length numbers)
  in
      List.concat [numbers, List.repeat numberOfZeroes 0]


squashRowRight : List Int -> List Int
squashRowRight = List.reverse >> squashRowLeft >> List.reverse


-- squoshing the whole grid

squashLeft : Matrix Int -> Matrix Int
squashLeft = Matrix.toList >> List.map squashRowLeft >> Matrix.fromList

squashRight : Matrix Int -> Matrix Int
squashRight = Matrix.toList >> List.map squashRowRight >> Matrix.fromList

squashDown : Matrix Int -> Matrix Int
squashDown = Matrix.transpose >> squashLeft >> Matrix.transpose

squashUp : Matrix Int -> Matrix Int
squashUp = Matrix.transpose >> squashRight >> Matrix.transpose


moveCells : { x: Int, y : Int } -> Matrix Int -> Matrix Int
moveCells input =
  case (input.x, input.y) of
    ( 1,  0) -> squashRight
    (-1,  0) -> squashLeft
    ( 0, -1) -> squashDown
    ( 0,  1) -> squashUp
    _ -> identity


update : { x : Int, y : Int } -> Matrix Int ->  Matrix Int
update input model = model |> moveCells input |> Matrix.set 0 0 2


gameState : Signal (Matrix Int)
gameState = Signal.foldp update (defaultGrid 4 4) Keyboard.arrows


view model = Views.Grid.render Config.defaultConfig (Debug.watch "Game State" model)


main = Signal.map view gameState
