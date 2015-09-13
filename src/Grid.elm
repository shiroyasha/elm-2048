module Grid where

import Cell
import Matrix
import Shapes
import Color

import Graphics.Collage exposing (..)


type alias Model =
  { cells: Matrix.Matrix Cell.Model
  , size: (Float, Float)
  }


init : (Float, Float) -> (Int, Int) -> Model
init size (rows, cols) =
  let
      cell x y = Cell.init ((toFloat x)*100.0, (toFloat y)*100.0) 90.0 2
  in
    { cells = Matrix.matrix rows cols cell, size = size }


-- UPDATE

type Action = Tick Float

update : Action -> Model -> Model
update action model =
  case action of
    Tick dt ->
      let
          cells' = Matrix.map (Cell.update (Cell.Tick dt)) model.cells
      in
        { model | cells <- cells' }


-- VIEW

view model =
  let
      bgColor = Color.rgb 187 173 160
      bgWidth = fst model.size |> round
      bgHeight = snd model.size |> round

      bg = Shapes.roundedRect bgWidth bgHeight 3 bgColor

      cells = group (Matrix.map Cell.view model.cells |> Matrix.flatten)
  in
     group [bg, cells]
