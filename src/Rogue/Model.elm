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

defaultGameMap : Int -> GameMap
defaultGameMap size = Matrix.initialize size (\_ -> Open {player = Nothing})

generateMap : GameMap -> List (GameMap -> GameMap) -> GameMap
generateMap startingMap mapTransformers =
  foldr (<|) startingMap mapTransformers

defaultGame : Game
defaultGame =
  let
    p = Player
    startLoc = (0,0)
    size = 10
    g = generateMap (defaultGameMap size)
          [ insertPlayer p startLoc
          , setBarriers (randomizeLocationsWithin size 11)
          ]
  in
    { gameMap = g
    , player = p
    }

--doGame : Location -> Location -> List Location -> 

insertPlayer : Player -> Location -> GameMap -> GameMap
insertPlayer p here gm =
  Matrix.mapWithLocation (
    \location cell -> if location == here then Open {player = Just p} else cell
  ) gm

setBarriers : List Location -> GameMap -> GameMap
setBarriers barrierLocations gm =
  Matrix.mapWithLocation (
    \location cell -> if | doesContainPlayer cell -> cell
                         | location `member` barrierLocations -> Barrier {}
                         | otherwise -> cell
  ) gm

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
