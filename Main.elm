import Config
import Random exposing (Seed)
import Keyboard
import Time exposing (..)
import Debug

import Views.Grid
import Models.Grid exposing (Grid)
import Models.GameState exposing (GameState)

-- port startTime : Float
startTime = 5

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


keyboard: Signal Models.Grid.Action
keyboard =
  let
      toAction {x, y} = case (x, y) of
        ( 1,  0) -> Models.Grid.SquashRight
        (-1,  0) -> Models.Grid.SquashLeft
        ( 0, -1) -> Models.Grid.SquashUp
        ( 0,  1) -> Models.Grid.SquashDown
        _ -> Models.Grid.NoAction
  in
     Signal.map toAction Keyboard.arrows


gameState : Signal GameState
gameState = Signal.foldp Models.GameState.update (Models.GameState.initial startTimeSeed) keyboard


view {grid, seed} = Views.Grid.render Config.defaultConfig grid

main = Signal.map view gameState
