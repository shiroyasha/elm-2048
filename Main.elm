import Config
import Random exposing (Seed)

import Views.Grid exposing (render)
import Models.GameState exposing (GameState, update, initial)
import Input exposing (keyboard)

-- port startTime : Float
startTime = 5

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


gameState : Signal GameState
gameState = Signal.foldp update (initial startTimeSeed) Input.keyboard


view = render Config.defaultConfig


main = Signal.map (\{grid, seed} -> view grid) gameState
