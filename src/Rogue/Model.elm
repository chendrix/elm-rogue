module Rogue.Model where

import Array exposing (..)
import List exposing (member)
import Maybe exposing (..)
import Random exposing (..)

import Now

type alias Location = (Int, Int)

type alias GameMap =
  { board : Board
  , start : Location
  , currentPlayerLocation : Location
  }

type alias Board = Array (Array Cell)

type Player = Player

type alias Game =
  { gameMap : GameMap
  , player : Player
  }

type Cell = Open Location | Barrier Location

isOpen : Cell -> Bool
isOpen cell =
  case cell of
    (Open _) -> True
    (Barrier _) -> False

cellAt : Location -> Board -> Maybe Cell
cellAt (rowNum,colNum) board =
  get rowNum board `andThen` (\row -> get colNum row)

isAt : Location -> Cell -> Bool
isAt queried cell = queried == (loc cell)

loc : Cell -> Location
loc c =
  case c of
    Open l -> l
    Barrier l -> l

newBoardWithBarriersAt : Int -> List Location -> Board
newBoardWithBarriersAt size barrierLocations =
  initialize size (
    \row -> initialize size (
      \col -> if (row,col) `member` barrierLocations then Barrier (row, col) else Open (row, col)))
        
newBoard : Int -> Board
newBoard size =
  newBoardWithBarriersAt size []

randomizeLocationsWithin : Int -> Int -> List Location
randomizeLocationsWithin size numLocations =
  let 
    locationGenerator = list numLocations (pair (int 0 size) (int 0 size))
  in
    generate locationGenerator (initialSeed (round Now.loadTime)) |> fst

gameMap : Int -> GameMap
gameMap size  =
  let startLoc = (0,0) in
  { board = newBoardWithBarriersAt size (randomizeLocationsWithin size 11)
  , start = startLoc
  , currentPlayerLocation = startLoc
  }

defaultGame : Game
defaultGame =
  let
    g = gameMap 10
  in
    { gameMap = g
    , player = Player
    }

type alias Dir =
  { x : Int
  , y : Int
  }

type alias Input =
  { dir : Dir
  }

numRows : Board -> Int
numRows b = length b

numCols : Board -> Int
numCols = numRows

within : Board -> Location -> Bool
within board (row, col) =
  row >= 0 && row < numRows board && col >= 0 && col < numCols board

translate : Location -> Dir -> Location
translate (row,col) dir =
  (row - dir.y, col + dir.x)