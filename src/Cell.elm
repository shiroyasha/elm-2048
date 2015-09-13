module Cell where

import Shapes
import Graphics.Collage exposing (..)
import Text
import Color
import Time
import Animation
import Easing

-- MODEL

type alias Position = (Float, Float)
type alias Size = Float

type State
  = Moving Position Time.Time
  | Appearing Time.Time
  | Stationary

type alias Model =
  { number : Int
  , position : Position
  , size: Size
  , state: State
  }


init : Position -> Size -> Int -> Model
init position size number =
  { number = number, position = position, size = size, state = Appearing 0.0 }


appearingAnimation
   = Animation.animation 0
  |> Animation.from 0
  |> Animation.to 1
  |> Animation.duration (Time.second/2)
  |> Animation.ease (Easing.easeOutBack)


-- UPDATE

type Action = Move Position | Tick Float


update : Action -> Model -> Model
update action model =
  case action of
    Move (x, y) ->
      { model | state <- Moving (x, y) 0.0 }

    Tick dt ->
      case model.state of
        Stationary ->
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
            { model | state <- Moving toPosition time' }


-- VIEW

view : Model -> Form
view model =
  let
      bg = Shapes.roundedSquare (model.size |> round) 3 (Color.rgb 100 100 100)
      fg = model.number |> toString |> Text.fromString |> text

      cell = group [bg, fg] |> move model.position
  in
     case model.state of
       Stationary ->
         cell

       Moving toPosition time ->
         cell

       Appearing time ->
         scale (Animation.animate time appearingAnimation) cell
