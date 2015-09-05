module Models.Grid where

import Matrix exposing (Matrix)

type alias Grid = Matrix Int

grid : Int -> Int -> Grid
grid width height = Matrix.repeat width height 0 |> Matrix.set 1 1 2
