module Models.GameState where

import Random exposing (Seed)
import Models.Grid exposing (..)


type alias GameState =
  { grid : Grid
  , seed : Seed
  }


nth: Int -> List a -> Maybe a
nth index list =
  case index of
    0 -> List.head list
    _ -> nth (index - 1) (Maybe.withDefault [] <| List.tail list)


initial seed = { grid = Models.Grid.grid 4 4, seed = seed }


update : Models.Grid.Action -> GameState -> GameState
update gridAction {grid, seed} =
  let
      grid' = Models.Grid.update gridAction grid

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
