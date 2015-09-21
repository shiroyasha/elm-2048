module Cell where

import Graphics.Collage as Collage exposing (..)
import Units exposing (..)

import Shapes
import CellAnimations
import Time
import MatrixLayout

-- MODEL

type State
  = Moving MatrixPosition Time.Time
  | Appearing Time.Time
  | Stationary
  | WaitingForMerge MatrixPosition

type alias Model =
  { number : Int
  , matrixPosition : MatrixPosition
  , state: State
  }


init : Int -> MatrixPosition -> Model
init number matrixPosition =
  { number = number
  , matrixPosition = matrixPosition
  , state = if number == 0 then Stationary else Appearing 0.0
  }

-- UPDATE

moveTick matrixPosition time dt model =
  let
     time' = time + dt
  in
     if CellAnimations.moveFinished time'
        then { model | state <- WaitingForMerge matrixPosition }
        else { model | state <- Moving matrixPosition time' }


appeatTick time dt model =
  let
     time' = time + dt
  in
     if CellAnimations.appearFinished time'
        then { model | state <- Stationary }
        else { model | state <- Appearing time' }


tick dt model = case model.state of
  Appearing time ->
    appeatTick time dt model

  Moving matrixPosition time ->
    moveTick matrixPosition time dt model

  _ ->
    model

type Action = Move MatrixPosition | Tick Float | Empty | Add Int | Substract Int

update : Action -> Model -> Model
update action model = case action of
  Move matrixPosition ->
    if model.number == 0 || matrixPosition == model.matrixPosition
      then model
      else { model | state <- Moving matrixPosition 0.0 }

  Substract number ->
    case model.state of
      Appearing progress -> { model | number <- model.number - number }
      _ -> { model | state <- Stationary, number <- model.number - number }

  Add number ->
    if model.number == 0
      then { model | state <- Stationary, number <- number }
      else { model | state <- Appearing 200, number <- model.number + number }

  Tick dt ->
    tick dt model


-- VIEW


view : MatrixLayout.Model -> Model -> Form
view layout model =
  let
    position = MatrixLayout.cellPosition layout model.matrixPosition
    size = layout.cellSize

    cell = Shapes.cell size model.number |> Collage.move position
  in
     case model.state of
       Stationary ->
         cell

       WaitingForMerge matrixPosition ->
         cell

       Moving matrixPosition time ->
         let
           progress = CellAnimations.moveValue time
           toPosition = MatrixLayout.cellPosition layout matrixPosition

           diffX = ((fst toPosition) - (fst position)) * progress
           diffY = ((snd toPosition) - (snd position)) * progress

         in
           cell |> Collage.move (diffX, diffY)

       Appearing time ->
         scale (CellAnimations.appearValue time) cell
