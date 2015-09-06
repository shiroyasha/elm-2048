module Models.GridRow where

import List.Extra

groupPairs : List Int -> List (List Int)
groupPairs list =
  case list of
    [] -> []
    [x] -> [[x]]
    (x::y::xs) -> if x == y then [x, y] :: groupPairs xs else [x] :: groupPairs (y::xs)


points: List (List Int) -> Int
points list = list |> List.filter (\list -> (List.length list) > 1) |> List.map List.sum |> List.sum


filterNumbers : List Int -> List Int
filterNumbers = List.filter (\el -> el /= 0)


fill: Int -> List Int -> List Int
fill len row = row ++ List.repeat (len - (List.length row)) 0


squash : List Int -> (Int, List Int)
squash list =
  let
      groups = list |> filterNumbers |> groupPairs

      points' = groups |> points
      list'   = groups |> List.map List.sum |> fill (List.length list)
  in
      (points', list')
