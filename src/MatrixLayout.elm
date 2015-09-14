module MatrixLayout where

import Units exposing (..)

type alias Model =
  { cellSize : Float
  , cellPadding : Float
  , cols: Int
  , rows: Int
  , width: Float
  , height: Float
  }

cellPaddingRatio = 10

cellPadding : (Float, Float) -> (Int, Int) -> Float
cellPadding (width, height) (rows, cols) =
  width / (2 * ((cols |> toFloat)*(cellPaddingRatio + 1) + 1))

cellSize : Float -> Float
cellSize padding = cellPaddingRatio * 2 * padding


cellSizeWithPadding : Model -> Float
cellSizeWithPadding model = 2 * model.cellPadding + model.cellSize


cellPosition : Model -> MatrixPosition -> Position
cellPosition model (x, y) =
  let
      x' = x |> toFloat
      y' = y |> toFloat

      x'' = model.cellPadding + (cellSizeWithPadding model) * (x' + 1/2) - model.width/2
      y'' = model.cellPadding + (cellSizeWithPadding model) * (y' + 1/2) - model.height/2
  in
     (x'', y'')


gridSize : Model -> Size
gridSize model = (model.width, model.height)


init : (Float, Float) -> (Int, Int) -> Model
init (width, height) (rows, cols) =
  let
      padding = cellPadding (width, height) (rows, cols)
      cell = cellSize padding
  in
     { cellSize = cell
     , cellPadding = padding
     , cols = cols
     , rows = rows
     , width = width
     , height = height
     }

