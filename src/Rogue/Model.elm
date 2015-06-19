module Rogue.Model where

import Array exposing (..)
import List exposing (..)
import Maybe exposing (..)
import Random exposing (..)

import Now

type alias Location = (Int, Int)

type alias GameMap = Array (Array Cell)

type Player = Player

type alias Game =
  { gameMap : GameMap
  , player : Player
  }

type Cell = Open { player : Maybe Player } | Barrier {}

type alias Dir =
  { x : Int
  , y : Int
  }

type alias Input =
  { dir : Dir
  }

newBoardWithBarriersAt : Int -> List Location -> Player -> GameMap
newBoardWithBarriersAt size barrierLocations p =
  initialize size (
    \row -> initialize size (
      \col -> if | (row,col) == (0,0) -> Open { player = Just p }
                 | (row,col) `member` barrierLocations -> Barrier {}
                 | otherwise -> Open { player = Nothing }))


randomizeLocationsWithin : Int -> Int -> List Location
randomizeLocationsWithin size numLocations =
  let 
    locationGenerator = list numLocations (pair (int 0 size) (int 0 size))
  in
    generate locationGenerator (initialSeed (round Now.loadTime)) |> fst

gameMap : Int -> Player -> GameMap
gameMap size p = 
  newBoardWithBarriersAt size (randomizeLocationsWithin size 11) p

defaultGame : Game
defaultGame =
  let
    p = Player
    g = gameMap 10 p
  in
    { gameMap = g
    , player = p
    }

currentPlayerLocation : GameMap -> Maybe Location
currentPlayerLocation gameMap =
  let
    matrixOfLocAndHasPlayer =
      Array.indexedMap (
        \rowNum row ->
          Array.indexedMap (
            \colNum cell -> ((rowNum,colNum), doesContainPlayer cell)
          ) row
        ) gameMap
  in
    Array.map toList matrixOfLocAndHasPlayer
    |> toList
    |> concat
    |> List.filter snd
    |> List.map fst
    |> head

doesContainPlayer : Cell -> Bool
doesContainPlayer c =
  case c of
    Open {player} -> isJust player
    otherwise -> False

isJust : Maybe a -> Bool
isJust m =
  case m of
    Just _ -> True
    Nothing -> False

translate : Dir -> Location -> Location
translate  dir (row,col)=
  (row - dir.y, col + dir.x)

cellAt : GameMap -> Location ->  Maybe Cell
cellAt gameMap (rowNum, colNum) =
  Array.get rowNum gameMap `andThen` get colNum