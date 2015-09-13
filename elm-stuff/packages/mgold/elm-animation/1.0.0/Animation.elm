module Animation (Animation, animation, static, animate, duration, speed, delay, ease, from, to, undo, retarget, getStart, getDuration, getSpeed, getDelay, getEase, getFrom, getTo, equals, velocity, timeElapsed, timeRemaining, isScheduled, isRunning, isDone) where

{-| A library for animating between two Float values. For example, animate a panel's width from 100px to 300px over 2
seconds, or make a button spin and grow on hover. Everything is a pure function, no signals in sight, so you can use it
easily within your architecture. You can also inspect animations to determine if they are still running and for how
long, and even smoothly retarget a different destination midflight.

The library encapsulates a 3-stage animation pipeline:
* **Timekeeping:** You are expected to maintain a running clock, which could be as simple as `Signal.foldp (+) 0 (Time.fps 30)`
    or part of your state and action types. You use this clock to create an animation and again to obtain the current
    value. You can also specify a delay and the duration for your animation.

* **Easing:** An easing function is what makes an animation come alive with acceleration and sometimes even elasticity.
    When setting the easing function for your animation, I recommend [Dan's
    library](http://package.elm-lang.org/packages/Dandandan/Easing/latest/Easing#easing-functions). (Dan also has your
    back if you need to interpolate pairs or colors with the output of this library.)

* **Interpolation:** It wouldn't be very useful is all animations went from 0 to 1 (the default), would it? You can
    specify values to animate `from` and `to`. Furthermore, you can set the average speed (distance between these two
    values per milisecond) instead of a duration.

Once you have your value at the current time, you can render it to any frontend you choose: Collage, Element, Html,
[turtles](http://package.elm-lang.org/packages/mgold/elm-turtle-graphics/latest)...

# Basic Usage

`animation` creates an animation starting at the given time (usually the current time). `animate` takes the current time
and an animation and produces the current value.

````elm
import Animation exposing (..)
import Time exposing (second)

myAnim = animation 0 |> from 100 |> to 300 |> duration (4*second) |> delay (1*second)
List.map (\t -> animate (t*second) myAnim) [0..6]
-- [100, 100, 129.29, 200, 270.71, 300, 300]
````
Notice that the value remains constant during the delay and after the animation is done, and that sinusoidal easing
in and out is applied by default. Animations go through three phases (not related to the three stages of rendering):
they are scheduled, they run, and then they are done.

# Create
@docs animation, static

# Run
@docs animate

# Modify
## Settings
You may set an animation's duration or speed but not both, since one determines the other.
@docs duration, speed, delay, ease, from, to

## Interruptions
@docs undo, retarget

# Inspect
## Equality
@docs equals

## Lifecycle
@docs isScheduled, isRunning, isDone

## Physics
@docs timeElapsed, timeRemaining, velocity

## Settings
@docs getStart, getDuration, getSpeed, getDelay, getEase, getFrom, getTo

# The Animation Type
@docs Animation

-}

import Time exposing (Time)
import Easing as Dan

-- private
type DurationOrSpeed = Duration Time | Speed Float

-- private
type alias AnimRecord = { start : Time , delay : Time , dos : DurationOrSpeed
                        , ramp : Maybe Float -- used for interruptions
                        , ease : Float -> Float
                        , from : Float , to : Float
                        }

{-| An Animation is an opaque type that represents a time-varying number (floating point value).
-}
type Animation = A AnimRecord

-- private
dur : DurationOrSpeed -> Float -> Float -> Time
dur dos from to =
    case dos of
        Duration t -> t
        Speed s -> (abs (to - from)) / s

-- private
spd : DurationOrSpeed -> Float -> Float -> Float
spd dos from to =
    case dos of
        Duration t -> (abs (to - from)) / t
        Speed s -> s

--private
defaultDuration : DurationOrSpeed
defaultDuration = Duration (750 * Time.millisecond)

{-| Create an animation that begins now. By default, animations have no delay, last 750ms, and interpolate between 0 and
1 with a sinusoidal easing function. All of these can be changed.
-}
animation : Time -> Animation
animation now = A <| AnimRecord now 0 defaultDuration Nothing Dan.easeInOutSine 0 1

{-| Create a static animation that is always the given value.
-}
static : Float -> Animation
static x = A <| AnimRecord 0 0 (Duration 0) Nothing Dan.easeInOutSine x x

{-| Produce the value of an animation at a given time.
-}
animate : Time -> Animation -> Float
animate t (A {start, delay, dos, ramp, from, to, ease})  =
    let duration = dur dos from to
        fr = clamp 0 1 <| (t - start - delay) / duration
        eased = ease fr
        correction = case ramp of
            Nothing -> 0
            Just vel -> let eased' = Dan.easeInOutSine fr -- always use cosine ease for this
                            from' = vel * (t - start)
                        in from' - from'*eased'
    in from + (to-from)*eased + correction

{-| Run an animation in reverse from its current state, beginning immediately (even if the animation was delayed or has
been done for a while).

Usually you don't want to undo an animation that has been retargeted; just retarget it again. Similarly, undoing an
undone animation is frequently not what you want.
-}
undo : Time -> Animation -> Animation
undo t (A a as u) =
    A {a| from <- a.to, to <- a.from, start <- t, delay <- -(timeRemaining t u), ramp <- Nothing,
          ease <- (\t -> 1 - (a.ease (1 - t)))}

{-| Change the `to` value of a running animation, without an abrupt change in velocity. The easing function will be
retained (but you can change it with `ease`). A new speed and duration will be chosen based on what makes the animation
smooth. If you retarget multiple animations at once (e.g. x and y), you will need to sync their durations (perhaps to
the `timeRemaining` in the old animations).

If the retargeted animation is still scheduled, the `to` value is replaced. If it is already done, `from` becomes the
old `to`, `to` and `start` are set to the values provided, and the delay is set to zero.
-}
retarget : Time -> Float -> Animation -> Animation
retarget t newTo (A a as u) =
    if | isScheduled t u -> A {a| to <- newTo, ramp <- Nothing}
       | isDone t u -> A {a| start <- t, from <- a.to, to <- newTo, delay <- 0, ramp <- Nothing}
       | a.from == a.to -> A {a| start <- t, to <- newTo, dos <- defaultDuration, ramp <- Nothing}
       | otherwise ->
            let vel = velocity t u
                pos = animate t u
            in A <| AnimRecord t 0 (Speed (vel/3)) (Just vel) a.ease pos newTo

{-| Set the duration of an animation to the time specified. This setting overrides, and is overriden by, `speed` (last
application wins). Note that the `Time` argument is _not_ the current running time but the duration to be set.
-}
duration : Time -> Animation -> Animation
duration x (A a) = A {a| dos <- Duration x}

{-| Set the _average_ speed of an animation. Speed is the rate at which the animation progresses between the `from` and
`to` values per milisecond. Most easing functions will deviate from the average speed. You do not need to worry about
the sign. It is safe to alter the `from` and `to` values after setting speed, but a set speed will be overwritten by
`duration`.
-}
speed : Float -> Animation -> Animation
speed x (A a) = A {a| dos <- Speed (abs x)}

{-| Set the delay of an animation to the time specified. An animation will not start until after the delay. The default
delay is 0. Note that the `Time` argument is _not_ the current running time but the delay to be set.
-}
delay : Time -> Animation -> Animation
delay x (A a) = A {a| delay <- x}

{-| Set the easing function of an animation. It is expected that `f 0 == 0` and `f 1 == 1`. The default is a sinusoidal
in-out.
-}
ease : (Float -> Float) -> Animation -> Animation
ease x (A a) = A {a| ease <- x}

{-| Set the initial value of an animation. The default is 0.
-}
from : Float -> Animation -> Animation
from x (A a) = A {a| from <- x, ramp <- Nothing}

{-| Set the final value of an animation. The default is 1.

For animations that are already running, use `retarget`.
-}
to : Float -> Animation -> Animation
to x (A a) = A {a| to <- x, ramp <- Nothing}

{-| Get the time elapsed since the animation started playing (after the end of delay). Will be zero for animations that
are still scheduled, and is not bounded for animations that are already done.
-}
timeElapsed : Time -> Animation -> Time
timeElapsed t (A {start, delay}) =
    t - (start + delay) |> max 0

{-| Get the time that the animation has yet to play (or be delayed) before becoming done. Will be zero for animations
that are already done.
-}
timeRemaining : Time -> Animation -> Time
timeRemaining t (A {start, delay, dos, from, to}) =
    let duration = dur dos from to
    in start+delay+duration - t |> max 0

{-| Get the _current_ velocity of the animation, aproximated by looking 10ms forwards and backwards (the central
difference). The velocity may be negative.
-}
velocity : Time -> Animation -> Float
velocity t u =
    let backDiff = animate (t-10) u
        forwDiff = animate (t+10) u
    in (forwDiff - backDiff) / 20

{-| Get the start time of the animation, the argument to `animate`. For interrupted animations, this is when the
interruption occured.
-}
getStart : Animation -> Time
getStart (A a) = a.start

{-| Get the duration of the animation, not counting delay.
-}
getDuration : Animation -> Time
getDuration (A {dos, from, to}) = dur dos from to

{-| Get the average speed of the animation.
-}
getSpeed : Animation -> Float
getSpeed (A {dos, from, to}) = spd dos from to

{-| Get the delay of the animation.
-}
getDelay : Animation -> Time
getDelay (A a) = a.delay

{-| Get the easing function of the animation.
-}
getEase : Animation -> Float -> Float
getEase (A a) = a.ease

{-| Get the initial value of the animation.
-}
getFrom : Animation -> Float
getFrom (A a) = a.from

{-| Get the final value of the animation.
-}
getTo : Animation -> Float
getTo (A a) = a.to

{-| Equality on animations. Compared to `(==)` (which should not be used), this
function handles the conversion of speed and duration, and start and delay. It
also samples the easing functions, which may produce false positives (but
usually not in practice).

    -- These are True
    animation 0 `equals` animation 0
    (animation 0 |> delay 10) `equals` animation 10
    (animation 0 |> duration 1000) `equals` (animation 0 |> speed 0.001)

    -- These are False
    static 0 `equals` animation 0
    (animation 0 |> from -1) `equals` animation 0
    (animation 0 |> ease identity) `equals` animation 0
-}
equals : Animation -> Animation -> Bool
equals (A a) (A b) =
    a.start + a.delay == b.start + b.delay &&
    a.from == b.from &&
    a.to == b.to &&
    a.ramp == b.ramp &&
    (a.dos == b.dos || 0.001 >= abs (dur a.dos a.from a.to - dur b.dos b.from b.to)) &&
    List.all (\t -> a.ease t == b.ease t) [0.1, 0.3, 0.7, 0.9]


{-| Determine if an animation is scheduled, meaning that it has not yet changed value.
-}
isScheduled : Time -> Animation -> Bool
isScheduled t (A {start, delay}) =
    t <= start+delay

{-| Determine if an animation is running, meaning that it is currently changing value.
-}
isRunning : Time -> Animation -> Bool
isRunning t (A {start, delay, dos, from, to}) =
    let duration = dur dos from to
    in t > start+delay && t < start+delay+duration

{-| Determine if an animation is done, meaning that it has arrived at its final value.
-}
isDone : Time -> Animation -> Bool
isDone t (A {start, delay, dos, from, to}) =
    let duration = dur dos from to
    in t >= start+delay+duration
