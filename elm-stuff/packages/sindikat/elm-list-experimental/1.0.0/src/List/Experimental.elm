module List.Experimental
  ( and, or, conjunction, disjunction
  , lookup, filter2, filterMap2, filterM
  ) where

{-| List.Experimental is a testing playground for various List related functions. It contains functions that are experimental, unidiomatic, controversial or downright silly. This is specifically to not clutter List and List.Extra, and also have an isolated place to test crazy ideas.

*Do not* use this module in production code. Try your best to come up with equivalent functionality or solve your problem in a different way, and if you fail, consider contributing to List and List.Extra packages.

*Do not* import functions from this module unqualified if you do use it.

This package has the lowest possible bar for inclusion of List related functions. If you have some code that you want to publish somewhere, but not necessarily contribute to core libraries, feel absolutely free to contribute here. Treat this package as a safe sandbox. The GitHub page for ideas, suggestions, discussions, and pull requests is:

https://github.com/sindikat/elm-list-experimental

# List functions
@docs and, or, conjunction, disjunction, lookup, filter2, filterMap2, filterM
-}

import List exposing (..)
import List.Extra exposing (..)


{-| Return the conjunction of all `Bool`s in a list. In other words, return True if all elements are True, return False otherwise. `and` is equivalent to `all identity`. Return True on an empty list.
-}
and : List Bool -> Bool
and = all identity


{-| Return the disjunction of all `Bool`s in a list. In other words, return True if any element is True, return False otherwise. `or` is equivalent to `any identity`. Return False on an empty list.
-}
or : List Bool -> Bool
or = any identity


{-| Same as `and`.
-}
conjunction : List Bool -> Bool
conjunction = and


{-| Same as `or`.
-}
disjunction : List Bool -> Bool
disjunction = or


{-| Look up a key in an association list, return corresponding value, wrapped in `Just`. If no value is found, return `Nothing`. If multiple values correspond to the same key, return the first found value.

    lookup 'a' [('a',1),('b',2),('c',3)] == Just 1
    lookup 'd' [('a',1),('b',2),('c',3)] == Nothing
    lookup 3 [(1,"John"),(1,"Paul"),(2,"Mary")] == Just "John"
-}
lookup : a -> List (a, b) -> Maybe b
lookup key = Maybe.map snd << find (\item -> fst item == key)


{-| Filter over two lists simultaneously using a custom comparison function, return a list of pairs.
-}
filter2 : (a -> b -> Bool) -> List a -> List b -> List (a,b)
filter2 p xs ys = filter (uncurry p) (map2 (,) xs ys)


{-| Remove all elements from both lists that don't satisfy the predicate, then apply the function.
-}
filterMap2 : (a -> b -> c) -> (a -> b -> Bool) -> List a -> List b -> List c
filterMap2 f p xs ys = map (uncurry f) <| filter2 p xs ys


{-| Filter that exploits the behavior of `andThen`.

Return all subsequences of a list:

    filterM (\x -> [True, False]) [1,2,3] == [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

Return all subsequences that contain 2:

    filterM (\x -> if x==2 then [True] else [True,False]) [1,2,3] == [[1,2,3],[1,2],[2,3],[2]]

-}
filterM : (a -> List Bool) -> List a -> List (List a)
filterM p =
  let
    andThen = flip concatMap
    go x r = p x `andThen`
             (\flg -> r `andThen`
                      (\ys -> [if flg then x::ys else ys]))
  in
    foldr go [[]]
