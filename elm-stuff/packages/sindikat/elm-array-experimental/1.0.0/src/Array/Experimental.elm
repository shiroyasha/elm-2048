module Array.Experimental
  ( get, set, update
  , take, drop
  ) where

{-| Array.Experimental is a testing playground for various Array related functions. It contains functions that are experimental, unidiomatic, controversial or downright silly. This is specifically to not clutter Array and Array.Extra, and also have an isolated place to test crazy ideas.

*Do not* use this module in production code. Try your best to come up with equivalent functionality or solve your problem in a different way, and if you fail, consider contributing to Array and Array.Extra packages.

*Do not* import functions from this module unqualified if you do use it.

This package has the lowest possible bar for inclusion of Array related functions. If you have some code that you want to publish somewhere, but not necessarily contribute to core libraries, feel absolutely free to contribute here. Treat this package as a safe sandbox. The GitHub page for ideas, suggestions, discussions, and pull requests is:

https://github.com/sindikat/elm-maybe-experimental

# Array functions
@docs get, set, update, take, drop
-}

import Array exposing (..)
import Array.Extra exposing (..)


{-| Return `Just` the element at the index. Given negative argument, counts the element from end. Returns `Nothing` if the index ≥ abs (length array).

    get  0 (fromList [0,1,2]) == Just 0
    get  2 (fromList [0,1,2]) == Just 2
    get  5 (fromList [0,1,2]) == Nothing
    get -1 (fromList [0,1,2]) == Just 2

`Array.get` from core libraries do not support negative arguments, but it might in the future.
-}
get : Int -> Array a -> Maybe a
get n a =
  let
    len = length a
  in
    if n < 0 && abs n <= len
    then Array.get (len + n) a
    else Array.get n a


{-| Sets the element at the index. Given negative argument, counts the element from end. Returns the array unchanged if the index ≥ abs (length array).

    set  1 7 (fromList [1,2,3]) == fromList [1,7,3]
    set -1 7 (fromList [1,2,3]) == fromList [1,2,7]

`Array.set` from core libraries do not support negative arguments, but it might in the future.
-}
set : Int -> a -> Array a -> Array a
set n x a =
  let
    len = length a
  in
    if n < 0 && abs n <= len
    then Array.set (len + n) x a
    else Array.set n x a


{-| INTERNAL FUNCTION. Updates an element of an array using a function. Returns `Nothing` on negative integer argument.
-}
updateOld : Int -> (a -> a) -> Array a -> Array a
updateOld n f a =
  let
    element = Array.get n a
  in
    case element of
      Nothing -> a
      Just element' -> Array.set n (f element') a


{-| Update the element at the index using a function. Given negative argument, counts the element from end. Returns the array unchanged if the index ≥ abs (length array).

    update  1 ((+)10) (fromList [1,2,3]) == fromList [1,12,3]
    update -1 ((+)10) (fromList [1,2,3]) == fromList [1,2,13]

There is no `Array.update` in the core libraries.
-}
update : Int -> (a -> a) -> Array a -> Array a
update n f a =
  let
    len = length a
  in
    if n < 0 && abs n <= len
    then updateOld (len + n) f a
    else updateOld n f a


{-| Take *n* first elements from an array. In other words, slice an array from the very beginning until index not including.

    take 5 (fromList [0..9]) == fromList [0,1,2,3,4]
-}
take : Int -> Array a -> Array a
take n a =
  if n >= 0
  then slice 0 n a
  else slice 0 (length a + n) a


{-| Drop *n* first elements from an array. In other words, slice an array from an index until the very end.

    drop 5 (fromList [0..9]) == fromList [5,6,7,8,9]
-}
drop : Int -> Array a -> Array a
drop n a = slice n (length a) a
