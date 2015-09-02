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
  |> Matrix.set 1 1 8


-- squoshing rows

squashRowLeft : List Int -> List Int
squashRowLeft list =
  let
      partitions = List.partition (\x -> x == 0) list

      zeroes  = fst partitions
      numbers = snd partitions
  in
      List.concat [numbers, zeroes]

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


update : { x : Int, y : Int } -> Matrix Int ->  Matrix Int
update input =
  case (input.x, input.y) of
    ( 1,  0) -> squashRight
    (-1,  0) -> squashLeft
    ( 0, -1) -> squashDown
    ( 0,  1) -> squashUp
    _ -> identity


gameState : Signal (Matrix Int)
gameState = Signal.foldp update (defaultGrid 4 4) Keyboard.arrows


view model = Views.Grid.render Config.defaultConfig (Debug.watch "Game State" model)


main = Signal.map view gameState
