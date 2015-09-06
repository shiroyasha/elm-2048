module Config where

type alias GridConfig = { padding: Int
                        , radius: Int
                        , rows: Int
                        , cols: Int}

defaultGridConfig : GridConfig
defaultGridConfig = { padding = 10
                    , radius = 5
                    , rows = 4
                    , cols = 4 }


type alias CellConfig = { padding: Int
                        , size: Int
                        , radius: Int }

defaultCellConfig : CellConfig
defaultCellConfig = { padding = 5
                    , radius = 5
                    , size = 110 }


type alias Config = { cell: CellConfig, grid: GridConfig }

defaultConfig : Config
defaultConfig = { cell = defaultCellConfig
                , grid = defaultGridConfig }
