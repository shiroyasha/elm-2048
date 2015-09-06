import Graphics.Element exposing (Element)
import ElmTest.Test exposing (suite, test, Test)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

import Models.Grid exposing (..)
import Matrix

emptyGridTests =
  suite "emptyGrid"
    [ test "produces an empty grid"
        <| assertEqual (emptyGrid 2 2) ([[0, 0], [0, 0]] |> Matrix.fromList)
    ]

gridTests =
  let
      testGrid = grid 4 3

      expectedStructure = Matrix.fromList
        [ [0, 0, 0, 0]
        , [0, 2, 0, 0]
        , [0, 0, 0, 0]
        ]
  in
     suite "grid"
       [ test "has the provided width"
           <| assertEqual (Matrix.width testGrid) 4
       , test "has the provided height"
           <| assertEqual (Matrix.height testGrid) 3
       , test "has one element set to 2"
           <| assertEqual (Matrix.get 1 1 testGrid) (Just 2)
       , test "has the expected structure"
           <| assertEqual testGrid expectedStructure
       ]


updateTests =
  let
      testGrid = Models.Grid.grid 3 3

      expectedStructure = Matrix.fromList
        [ [0, 0, 0]
        , [0, 2, 0]
        , [0, 0, 0]
        ]

      leftSquash = Matrix.fromList
        [ [0, 0, 0]
        , [2, 0, 0]
        , [0, 0, 0]
        ]

      rightSquash = Matrix.fromList
        [ [0, 0, 0]
        , [0, 0, 2]
        , [0, 0, 0]
        ]

      upSquash = Matrix.fromList
        [ [0, 2, 0]
        , [0, 0, 0]
        , [0, 0, 0]
        ]

      downSquash = Matrix.fromList
        [ [0, 0, 0]
        , [0, 0, 0]
        , [0, 2, 0]
        ]
  in
     suite "update"
       [ test "original grid has the expected structure"
           <| assertEqual testGrid expectedStructure
       , test "can squash numbers left"
           <| assertEqual (update SquashLeft testGrid) leftSquash
       , test "can squash numbers right"
           <| assertEqual (update SquashRight testGrid) rightSquash
       , test "can squash numbers up"
           <| assertEqual (update SquashUp testGrid) upSquash
       , test "can squash numbers down"
           <| assertEqual (update SquashDown testGrid) downSquash
       ]


squashTests =
  suite "squashRow"
    [ test "doesn't change the length of the list"
        <| assertEqual ([2, 2, 0] |> squashRowLeft |> List.length) 3
    , test "sums the numbers if they are equal"
        <| assertEqual ([2, 2, 0] |> squashRowLeft) [4, 0, 0]
    , test "doesn't sums the numbers if they are not equal"
        <| assertEqual ([16, 2, 0] |> squashRowLeft) [16, 2, 0]
    , test "sums the numbers even if they are not on the start of the list"
        <| assertEqual ([16, 2, 2] |> squashRowLeft) [16, 4, 0]
    ]

emptyPositionsTests =
  suite "emptyPositions"
    [ test "returns every empty position"
        <| assertEqual (grid 2 2 |> emptyPositions) [(0, 0), (1, 0), (0, 1)]
    ]


tests : Test
tests = suite "Models.Grid"
  [ emptyGridTests
  , gridTests
  , updateTests
  , squashTests
  , emptyPositionsTests
  ]

main : Element
main = runDisplay tests
