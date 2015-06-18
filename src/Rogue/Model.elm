module Rogue.Model where

import List exposing (..)

type alias Location = (Int, Int)

type alias GameMap =
  { board : Board
  , start : Location
  , currentPlayerLocation : Location
  }

type alias Board = List (List Cell)

type Player = Player

type alias Game =
  { gameMap : GameMap
  , player : Player
  }

type Cell = Open Location

isAt : Location -> Cell -> Bool
isAt queried (Open current) = queried == current

newBoard : Int -> Board
newBoard size =
  map (\row ->
        map
        (\col -> Open (row, col)
        ) [0..(size - 1)]) [0..(size - 1)]

gameMap : Int -> GameMap
gameMap size =
  let startLoc = (0,0) in
  { board = newBoard size
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