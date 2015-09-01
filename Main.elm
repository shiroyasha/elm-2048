import Matrix exposing (Matrix)

import Config
import Views.Grid

defaultGrid : Int -> Matrix Int
defaultGrid rows = Matrix.square rows (always 2)

main =
  let
      config = Config.defaultConfig
      model = defaultGrid config.grid.rows
  in
     Views.Grid.render config model
