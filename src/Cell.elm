module Cell where

import Shapes
import Graphics.Collage exposing (..)
import Text
import Color
import Time
import Animation
import Easing
import Units exposing (..)

-- MODEL

type State
  = Moving Position Time.Time
  | Appearing Time.Time
  | Stationary
  | Merging

type alias Model =
  { number : Int
  , position : Position
  , size: Float
  , state: State
  }


init : Position -> Float -> Int -> Model
init position size number =
  { number = number
  , position = position
  , size = size
  , state = if number == 0 then Stationary else Appearing 0.0
  }


appearingAnimation
   = Animation.animation 0
  |> Animation.from 0
  |> Animation.to 1
  |> Animation.duration (Time.second/2)
  |> Animation.ease (Easing.easeOutBack)

moveAnimation
   = Animation.animation 0
  |> Animation.from 0
  |> Animation.to 1
  |> Animation.duration (Time.second/2)
  |> Animation.ease (Easing.easeOutBack)


-- UPDATE

type Action = Move (Float, Float) | Tick Float


update : Action -> Model -> Model
update action model =
  case action of
    Move (x, y) ->
      { model | state <- Moving (x, y) 0.0 }

    Tick dt ->
      case model.state of
        Stationary ->
          model

        Merging ->
          model

        Appearing time ->
          let
             time' = time + dt
          in
             if Animation.isDone time' appearingAnimation
                then
                  { model | state <- Stationary }
                else
                  { model | state <- Appearing time' }

        Moving toPosition time ->
          let
             time' = time + dt
          in
             if Animation.isDone time' appearingAnimation
                then
                  { model | state <- Merging }
                else
                  { model | state <- Moving toPosition time' }


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
   |> move (0, 7)


labelSize : Float -> Int -> Float
labelSize cellSize number =
  if | number < 100 -> cellSize / 2
     | number > 100 && number < 1000 -> cellSize / 2.5
     | number > 1000  -> cellSize / 3

view : Model -> Form
view model =
  let
      bg = Shapes.roundedSquare model.size 3 (backgroungColor model.number)

      fgSize = labelSize model.size model.number
      fg = label fgSize model.number

      cell = group [bg, fg] |> move model.position
  in
     case model.state of
       Stationary ->
         cell

       Merging ->
         cell

       Moving toPosition time ->
         let
           progress = Animation.animate time moveAnimation

           diffX = ((fst toPosition) - (fst model.position)) * progress
           diffY = ((snd toPosition) - (snd model.position)) * progress

           base = Shapes.roundedSquare model.size 3 (backgroungColor 0) |> move model.position
         in
           group [base, cell |> move (diffX, diffY)]

       Appearing time ->
         scale (Animation.animate time appearingAnimation) cell
