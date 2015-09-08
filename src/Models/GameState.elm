module Models.GameState where

import Input
import Random exposing (Seed)
import Models.Grid exposing (..)
import Debug

type GamePhase = InProgress | GameOver | Won

type alias GameState =
  { grid : Grid
  , seed : Seed
  , score : Int
  , phase: GamePhase
  }

initial seed =
  { grid = Models.Grid.grid 4 4
  , seed = seed
  , score = 0
  , phase = InProgress
  }


lost : Grid -> Bool
lost grid =
  let
      movements = [SquashLeft, SquashRight, SquashUp, SquashDown]
      grids = List.map (\movement -> snd (Models.Grid.update movement grid)) movements
  in
     List.all ((==) grid) grids



won : Grid -> Bool
won grid = List.any ((==) 2048) (Models.Grid.numbers grid)


updatePhase: GameState -> GameState
updatePhase game =
  let
     phase' = if | won game.grid -> Won
                 | lost game.grid -> GameOver
                 | otherwise -> game.phase
  in
     { game | phase <- phase' }


applyAction: Models.Grid.Action -> GameState -> GameState
applyAction action game =
  let
     (points, grid') = Models.Grid.update action game.grid
  in
     { game | score <- (game.score + points), grid <- grid' }


addCell: GameState -> GameState -> GameState
addCell originalGame game =
  let
      (seed', grid') = if originalGame.grid == game.grid
                          then (game.seed, game.grid)
                          else Models.Grid.addRandomCell game.seed game.grid
  in
     { game | seed <- seed', grid <- grid' }


update : Input.Input -> GameState -> GameState
update input game =
  case input of
    Input.NewGame () -> initial game.seed
    Input.Movement action ->
      case action of
        NoAction -> game
        _ -> game |> applyAction action |> addCell game |> updatePhase
