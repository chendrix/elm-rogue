module Matrix where

import Array
import List
import Maybe exposing (..)

type alias Matrix a = Array.Array (Array.Array a)
type alias Location = (Int, Int)

initialize : Int -> (Location -> a) -> Matrix a
initialize size f =
  Array.initialize size (
    \row -> Array.initialize size (
      \col -> f (row,col)))

map : (a -> b) -> Matrix a -> Matrix b
map f m =
  Array.map (Array.map f) m

mapWithLocation : (Location -> a -> b) -> Matrix a -> Matrix b
mapWithLocation f m =
  Array.indexedMap (
    \rowNum row -> Array.indexedMap (
      \colNum cell -> 
        f (rowNum, colNum) cell
    ) row
  ) m 

toList : Matrix a -> List (List a)
toList m =
  Array.map Array.toList m
  |> Array.toList

flatten : Matrix a -> List a
flatten m =
  List.concat <| toList m 


cellAt : Matrix a -> Location ->  Maybe a
cellAt m (rowNum, colNum) =
  Array.get rowNum m `andThen` Array.get colNum