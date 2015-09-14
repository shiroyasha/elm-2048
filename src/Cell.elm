module Cell where

import Shapes
import Graphics.Collage as Collage exposing (..)
import Text
import Color
import Time
import Animation
import Easing
import Units exposing (..)

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


appearingAnimation
   = Animation.animation 0
  |> Animation.from 0
  |> Animation.to 1
  |> Animation.duration (Time.second/10)
  |> Animation.ease (Easing.easeOutCirc)

moveAnimation
   = Animation.animation 0
  |> Animation.from 0
  |> Animation.to 1
  |> Animation.duration (Time.second/5)
  |> Animation.ease (Easing.easeOutCirc)


-- UPDATE

moveTick matrixPosition toPosition time dt model =
  let
     time' = time + dt
  in
     if Animation.isDone time' moveAnimation
        then { model | state <- WaitingForMerge matrixPosition }
        else { model | state <- Moving matrixPosition toPosition time' }

appeatTick time dt model =
  let
     time' = time + dt
  in
     if Animation.isDone time' appearingAnimation
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

backgroungColor : Int -> Color.Color
backgroungColor number =
  case number of
    0 -> Color.rgb 204 192 179
    2 -> Color.rgb 238 228 218
    4 -> Color.rgb 236 224 200
    8 -> Color.rgb 241 176 120
    16 -> Color.rgb 235 140 82
    32 -> Color.rgb 243 123 96
    64 -> Color.rgb 233 89 55
    128 -> Color.rgb 242 216 106
    256 -> Color.rgb 231 191 41
    512 -> Color.rgb 231 191 41
    1024 -> Color.rgb 228 183 19
    2048 -> Color.rgb 238 195 3


textColor : Int -> Color.Color
textColor number =
  case number of
    2 -> Color.rgb 120 110 101
    4 -> Color.rgb 120 110 101
    _ -> Color.white


label : Float -> Int -> Form
label size number =
   (if number > 0 then toString number else "")
   |> Text.fromString
   |> Text.color (textColor number)
   |> Text.height size
   |> Text.bold
   |> text
   |> Collage.move (0, 7)


labelSize : Float -> Int -> Float
labelSize cellSize number =
  if | number < 100 -> cellSize / 2
     | number > 100 && number < 1000 -> cellSize / 2.5
     | number > 1000  -> cellSize / 3

viewBase : Model -> Form
viewBase model =
  Shapes.roundedSquare model.size 3 (backgroungColor 0) |> Collage.move model.position

view : Model -> Form
view model =
  let
      cell = if model.number /= 0
                then
                  let
                    bg = Shapes.roundedSquare model.size 3 (backgroungColor model.number)

                    fgSize = labelSize model.size model.number
                    fg = label fgSize model.number
                  in
                    group [bg, fg] |> Collage.move model.position
                else
                  rect 100 100 |> filled (Color.rgba 0 0 0 0)
  in
     case model.state of
       Stationary ->
         cell

       WaitingForMerge matrixPosition ->
         cell

       Moving matrixPosition toPosition time ->
         let
           progress = Animation.animate time moveAnimation

           diffX = ((fst toPosition) - (fst model.position)) * progress
           diffY = ((snd toPosition) - (snd model.position)) * progress

         in
           cell |> Collage.move (diffX, diffY)

       Appearing time ->
         scale (Animation.animate time appearingAnimation) cell
