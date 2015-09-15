-- import Config

-- import Input exposing (keyboard)

-- import Views.Grid

-- import Models.GameState exposing (GameState, update, initial)



-- gameState : Signal GameState
-- gameState = Signal.foldp update (initial startTimeSeed) Input.input

-- main = Signal.map view gameState


import Random exposing (Seed)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Units exposing (..)

import Input
import Grid

import Views.Score
import Views.Title
import Views.Objective
import Views.NewGame

-- port startTime : Float
startTime = 5


startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime


initial : Grid.Model
initial
  = Grid.init (500, 500) (4, 4) startTimeSeed
  |> Grid.addCell (1, 1) 2
  |> Grid.addCell (2, 2) 2
  |> Grid.addCell (1, 2) 2


gameState : Signal Grid.Model
gameState = Signal.foldp update initial Input.input


update : Input.Input -> Grid.Model -> Grid.Model
update input game =
  case input of
    Input.NewGame () ->
      initial
    Input.Movement dir ->
      Grid.update (Grid.Move dir) game
    Input.Tick dt ->
      Grid.update (Grid.Tick dt) game


view : Grid.Model -> Element
view game = flow down
  [ flow right [Views.Title.render, Views.Score.render 0]
  , flow right [Views.Objective.render, Views.NewGame.render Input.newGame]
  , collage 500 500 [Grid.view game]
  ]

main = Signal.map view gameState
