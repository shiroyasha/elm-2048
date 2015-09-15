module CellAnimations where

import Time
import Easing
import Animation

appear
   = Animation.animation 0
  |> Animation.from 0
  |> Animation.to 1
  |> Animation.duration (Time.second/10)
  |> Animation.ease (Easing.easeOutCirc)

move
   = Animation.animation 0
  |> Animation.from 0
  |> Animation.to 1
  |> Animation.duration (Time.second/5)
  |> Animation.ease (Easing.easeOutCirc)

appearFinished time = Animation.isDone time appear
moveFinished time = Animation.isDone time move

appearValue time = Animation.animate time appear
moveValue time = Animation.animate time appear
