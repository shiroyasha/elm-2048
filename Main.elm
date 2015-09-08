import Config
import Random exposing (Seed)
import Graphics.Element exposing (..)

import Input exposing (keyboard)

import Views.Grid
import Views.Score
import Views.Title
import Views.Objective
import Views.NewGame

import Models.GameState exposing (GameState, update, initial)


-- port startTime : Float
startTime = 5


startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


gameState : Signal GameState
gameState = Signal.foldp update (initial startTimeSeed) Input.input


view game = flow down
  [ flow right [Views.Title.render, Views.Score.render game.score]
  , flow right [Views.Objective.render, Views.NewGame.render Input.newGame]
  , Views.Grid.render Config.defaultConfig game
  ]


main = Signal.map view gameState
