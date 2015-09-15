module Cell where

import Graphics.Collage as Collage exposing (..)
import Units exposing (..)

import Shapes
import CellAnimations
import Time

-- MODEL

type State
  = Moving MatrixPosition Position Time.Time
  | Appearing Time.Time
  | Stationary
  | WaitingForMerge MatrixPosition

type alias Model =
  { number : Int
  , position : Position
  , matrixPosition : MatrixPosition
  , size: Float
  , state: State
  }


init : Position -> MatrixPosition -> Float -> Int -> Model
init position matrixPosition size number =
  { number = number
  , position = position
  , matrixPosition = matrixPosition
  , size = size
  , state = if number == 0 then Stationary else Appearing 0.0
  }

-- UPDATE

moveTick matrixPosition toPosition time dt model =
  let
     time' = time + dt
  in
     if CellAnimations.moveFinished time'
        then { model | state <- WaitingForMerge matrixPosition }
        else { model | state <- Moving matrixPosition toPosition time' }


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

  Moving matrixPosition toPosition time ->
    moveTick matrixPosition toPosition time dt model

  _ ->
    model

type Action = Move MatrixPosition Position | Tick Float | Empty | Add Int | Substract Int

update : Action -> Model -> Model
update action model = case action of
  Move matrixPosition position ->
    if model.number == 0 || matrixPosition == model.matrixPosition
      then model
      else { model | state <- Moving matrixPosition position 0.0 }

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


view : Model -> Form
view model =
  let
      cell = if model.number /= 0
                then Shapes.cell model.size model.number |> Collage.move model.position
                else Shapes.emptyCell
  in
     case model.state of
       Stationary ->
         cell

       WaitingForMerge matrixPosition ->
         cell

       Moving matrixPosition toPosition time ->
         let
           progress = CellAnimations.moveValue time

           diffX = ((fst toPosition) - (fst model.position)) * progress
           diffY = ((snd toPosition) - (snd model.position)) * progress

         in
           cell |> Collage.move (diffX, diffY)

       Appearing time ->
         scale (CellAnimations.appearValue time) cell
