module Models.GameState where

import Random exposing (Seed)
import Models.Grid exposing (..)


type alias GameState =
  { grid : Grid
  , seed : Seed
  , score : Int
  }


nth: Int -> List a -> Maybe a
nth index list =
  case index of
    0 -> List.head list
    _ -> nth (index - 1) (Maybe.withDefault [] <| List.tail list)


initial seed = { grid = Models.Grid.grid 4 4, seed = seed, score = 0}


addRandomCell: Seed -> Grid -> (Seed, Grid)
addRandomCell seed grid =
  let
    (randomNumber, seed') = Random.generate (Random.int 0 100) seed

    emptyPositions = Models.Grid.emptyPositions grid

    randomPosition = nth (randomNumber % (List.length emptyPositions)) emptyPositions

    grid' = case randomPosition of
      Just position -> Models.Grid.addCell position grid
  in
     (seed', grid')



update : Models.Grid.Action -> GameState -> GameState
update gridAction {grid, seed, score} =
  let
      (points, grid') = if gridAction /= Models.Grid.NoAction
                           then Models.Grid.update gridAction grid
                           else (0, grid)

      (seed', grid'') = if grid == grid'
                           then (seed, grid')
                           else addRandomCell seed grid'
  in
    { grid = grid'', seed = seed', score = score + points}
