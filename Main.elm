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


type alias Direction = {x: Int, y: Int}
type alias Input = Direction


toAction : Direction -> Models.Grid.Action
toAction {x, y} = case (x, y) of
  ( 1,  0) -> Models.Grid.SquashRight
  (-1,  0) -> Models.Grid.SquashLeft
  ( 0, -1) -> Models.Grid.SquashUp
  ( 0,  1) -> Models.Grid.SquashDown
  _ -> Models.Grid.NoAction


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
      action = toAction input
      grid' = Models.Grid.update action grid

      (position, seed') = Debug.watch "Randomness" <| Random.generate (Random.int 1 16) seed

      grid'' = if grid /= grid'
                  then
                    case randomEmptyPosition position grid' of
                      Just (x, y, number) -> Matrix.set x y 2 grid'
                      Nothing -> grid'
                  else
                    grid'
  in
    { grid = grid'', seed = seed' }


gameState : Signal GameState
gameState =
  let
      initialGameState = { grid = Models.Grid.grid 4 4, seed = startTimeSeed }
  in
      Signal.foldp update initialGameState Keyboard.arrows


view {grid, seed} = Views.Grid.render Config.defaultConfig (Debug.watch "Number Grid" grid)

main = Signal.map view gameState
