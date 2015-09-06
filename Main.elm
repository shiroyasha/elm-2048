import Config
import Random exposing (Seed)
import Graphics.Element exposing (..)

import Input exposing (keyboard)

import Views.Grid
import Views.Score
import Views.Title
import Views.Objective

import Models.GameState exposing (GameState, update, initial)


-- port startTime : Float
startTime = 5

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


gameState : Signal GameState
gameState = Signal.foldp update (initial startTimeSeed) Input.keyboard


view grid score = flow down
  [ flow right [Views.Title.render, Views.Score.render score]
  , Views.Objective.render
  , Views.Grid.render Config.defaultConfig grid
  ]


main = Signal.map (\{grid, score} -> view grid score) gameState
