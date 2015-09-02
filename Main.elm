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


squashLeft : List Int -> List Int
squashLeft list = List.concat [List.drop 1 list, List.take 1 list]


squashRight : List Int -> List Int
squashRight list = List.concat [List.drop 3 list, List.take 3 list]


update : { x : Int, y : Int } -> Matrix Int ->  Matrix Int
update input state =
  case (input.x, input.y) of
    ( 1,  0) -> state |> Matrix.toList |> List.map squashRight |> Matrix.fromList
    (-1,  0) -> state |> Matrix.toList |> List.map squashLeft |> Matrix.fromList
    ( 0,  1) -> state |> Matrix.transpose |> Matrix.toList |> List.map squashRight |> Matrix.fromList |> Matrix.transpose
    ( 0, -1) -> state |> Matrix.transpose |> Matrix.toList |> List.map squashLeft |> Matrix.fromList |> Matrix.transpose
    _ -> state


gameState : Signal (Matrix Int)
gameState = Signal.foldp update (defaultGrid 4 4) Keyboard.arrows


view model = Views.Grid.render Config.defaultConfig (Debug.watch "Game State" model)


main = Signal.map view gameState
