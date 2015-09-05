import Config
import Random exposing (Seed)
import Keyboard
import Time exposing (..)
import Debug

import Matrix exposing (Matrix)

import Views.Grid
import Models.Grid

-- port startTime : Float
startTime = 5

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


type alias GameState =
  { grid : Matrix Int
  , seed : Random.Seed
  }


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


type alias Direction = {x: Int, y: Int}
type alias Input = Direction


moveCells : Direction -> Matrix Int -> Matrix Int
moveCells {x, y} =
  case (x, y) of
    ( 1,  0) -> squashRight
    (-1,  0) -> squashLeft
    ( 0, -1) -> squashDown
    ( 0,  1) -> squashUp
    _ -> identity


movement : Direction -> Bool
movement {x, y} = List.member (x, y) [(1, 0), (-1, 0), (0, -1), (0, 1)]


randomEmptyPosition: Int -> Matrix Int -> Maybe (Int, Int, Int)
randomEmptyPosition randomNumber grid =
  let
    emptyPositions = grid |> Matrix.toIndexedList |> List.filter (\(_, _, number) -> number == 0)
  in
    emptyPositions |> List.drop (randomNumber % (List.length emptyPositions)) |> List.head


update : Input -> GameState -> GameState
update input {grid, seed} =
  let
      grid' = grid |> moveCells input

      (position, seed') = Debug.watch "Randomness" <| Random.generate (Random.int 1 16) seed

      grid'' = case randomEmptyPosition position grid' of
        Just (x, y, number) -> Matrix.set x y 2 grid'
        Nothing -> grid'
  in
     if movement input
        then { grid = grid'', seed = seed' }
        else { grid = grid, seed = seed }


gameState : Signal GameState
gameState = Signal.foldp update { grid = Models.Grid.grid 4 4, seed = startTimeSeed } Keyboard.arrows


view {grid, seed} = Views.Grid.render Config.defaultConfig (Debug.watch "Number Grid" grid)


main = Signal.map view gameState
