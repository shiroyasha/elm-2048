import Matrix exposing (Matrix)

import Config
import Views.Grid
import Random
import Keyboard
import Time exposing (..)

type alias Input = { space : Bool }

defaultGrid : Int -> Matrix Int
defaultGrid rows = Matrix.square rows (always 2)


update : Input -> Matrix Int ->  Matrix Int
update input state =
  let
      size = Matrix.rowCount state
      number = case Matrix.get (Matrix.loc 0 0) state of
                 Just n -> n + 1
                 Nothing -> 0
  in
    Matrix.square size (always number)


gameState : Signal (Matrix Int)
gameState = Signal.foldp update (defaultGrid 4) input


input : Signal Input
input = Signal.map Input Keyboard.space


view model = Views.Grid.render Config.defaultConfig model


main = Signal.map view gameState
