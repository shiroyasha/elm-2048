-- import Config
-- import Random exposing (Seed)

-- import Input exposing (keyboard)

-- import Views.Grid
-- import Views.Score
-- import Views.Title
-- import Views.Objective
-- import Views.NewGame

-- import Models.GameState exposing (GameState, update, initial)


-- -- port startTime : Float
-- startTime = 5


-- startTimeSeed : Seed
-- startTimeSeed = Random.initialSeed <| round startTime


-- gameState : Signal GameState
-- gameState = Signal.foldp update (initial startTimeSeed) Input.input


-- view game = flow down
--   [ flow right [Views.Title.render, Views.Score.render game.score]
--   , flow right [Views.Objective.render, Views.NewGame.render Input.newGame]
--   , Views.Grid.render Config.defaultConfig game
--   ]


-- main = Signal.map view gameState


import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Time exposing (..)

import Grid

initial : Grid.Model
initial
  = Grid.init (500, 500) (4, 4)
  |> Grid.addCell (1, 1) 2
  |> Grid.addCell (2, 2) 2
  |> Grid.addCell (1, 2) 2


gameState : Signal Grid.Model
gameState = Signal.foldp update initial (Time.fps 60)

update : Float -> Grid.Model -> Grid.Model
update dt game = Grid.update (Grid.Tick dt) game

view : Grid.Model -> Element
view game = collage 500 500 [Grid.view game]

main = Signal.map view gameState
