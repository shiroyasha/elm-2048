module Input where

import Keyboard
import Models.Grid exposing (Grid)

import Signal exposing ((<~))

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


newGame: Signal.Mailbox ()
newGame = Signal.mailbox ()


type Input = NewGame () | Movement Models.Grid.Action


input: Signal Input
input = Signal.merge (NewGame <~ newGame.signal) (Movement <~ keyboard)
