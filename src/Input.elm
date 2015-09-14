module Input where

import Keyboard
import Signal exposing ((<~))
import Units exposing (..)
import Time exposing (..)
import AnimationFrame

delta: Signal Time
delta = AnimationFrame.frame

newGame: Signal.Mailbox ()
newGame = Signal.mailbox ()

keyboard: Signal Direction
keyboard =
  let
    toDirection {x, y} = case (x, y) of
      ( 1,  0) -> Right
      (-1,  0) -> Left
      ( 0, -1) -> Up
      ( 0,  1) -> Down
      _ -> Up
  in
     Signal.map toDirection Keyboard.arrows

type Input
  = NewGame ()
  | Movement Direction
  | Tick Time

input: Signal Input
input =
  Signal.mergeMany [ NewGame <~ newGame.signal
                   , Movement <~ keyboard
                   , Tick <~ delta
                   ]
