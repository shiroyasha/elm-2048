import Config
import Random exposing (Seed)
import Keyboard
import Time exposing (..)
import Debug

import Matrix exposing (Matrix)

import Views.Grid
import Models.Grid exposing (Grid)

-- port startTime : Float
startTime = 5

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


type alias GameState =
  { grid : Matrix Int
  , seed : Random.Seed
  }

type alias Input = {x: Int, y: Int}


toAction : Input -> Models.Grid.Action
toAction {x, y} = case (x, y) of
  ( 1,  0) -> Models.Grid.SquashRight
  (-1,  0) -> Models.Grid.SquashLeft
  ( 0, -1) -> Models.Grid.SquashUp
  ( 0,  1) -> Models.Grid.SquashDown
  _ -> Models.Grid.NoAction



nth: Int -> List a -> Maybe a
nth index list =
  case index of
    0 -> List.head list
    _ -> nth (index - 1) (Maybe.withDefault [] <| List.tail list)


update : Input -> GameState -> GameState
update input {grid, seed} =
  let
      action = toAction input
      grid' = Models.Grid.update action grid

      emptyPositions = Models.Grid.emptyPositions grid'

      (randomNumber, seed') = Random.generate (Random.int 0 (List.length emptyPositions)) seed

      randomPosition = nth randomNumber emptyPositions

      grid'' = if grid /= grid'
                  then
                    case randomPosition of
                      Just position -> Models.Grid.addCell position grid'
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
