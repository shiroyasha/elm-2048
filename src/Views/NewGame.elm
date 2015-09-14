module Views.NewGame where

import Text exposing (..)
import Color exposing (..)
import Shapes exposing (..)

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (..)

title =
  "New Game"
  |> Text.fromString
  |> Text.color white
  |> Text.height 18
  |> Text.bold
  |> text
  |> move (0, 2)


basicButton color =
  collage 140 50 [Shapes.roundedRect (140, 40) 3 color, title]


render: Signal.Mailbox () -> Element
render mailbox =
  customButton (Signal.message mailbox.address ())
        (basicButton (rgb 120 110 101))
        (basicButton (rgb 130 120 111))
        (basicButton (rgb 120 110 101))

