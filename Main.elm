import Config
import Random exposing (Seed)

import Views.Grid exposing (render)
import Models.GameState exposing (GameState, update, initial)
import Input exposing (keyboard)

import Graphics.Element exposing (..)

-- port startTime : Float
startTime = 5

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


gameState : Signal GameState
gameState = Signal.foldp update (initial startTimeSeed) Input.keyboard


view grid score = flow down [show score, render Config.defaultConfig grid]


main = Signal.map (\{grid, score} -> view grid score) gameState
