module Rogue.Model where

import List exposing (..)
import Maybe exposing (..)
import Random exposing (..)
import Matrix exposing (..)

import Now

type alias GameMap = Matrix Cell

type Player = Player

type alias Game =
  { gameMap : GameMap
  , player : Player
  }

type Item = Item

type Cell 
  = Open  { player : Maybe Player 
          } 
  | Barrier {}

type alias Dir =
  { x : Int
  , y : Int
  }

type alias Input =
  { dir : Dir
  }

randomizeLocationsWithin : Int -> Int -> List Location
randomizeLocationsWithin size numLocations =
  let 
    locationGenerator = list numLocations (pair (int 0 size) (int 0 size))
  in
    generate locationGenerator (initialSeed (round Now.loadTime)) |> fst

gameMap : Int -> Player -> GameMap
gameMap size p = 
  let
    barrierLocations = randomizeLocationsWithin size 11
  in
    Matrix.initialize size (
      \loc -> if | loc == (0,0) -> Open { player = Just p }
                 | loc `member` barrierLocations -> Barrier {}
                 | otherwise -> Open { player = Nothing }
    )

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
      mapWithLocation (
        \loc cell -> (loc, doesContainPlayer cell)
      ) gameMap
  in
    matrixOfLocAndHasPlayer
    |> flatten
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
