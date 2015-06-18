module Rogue where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Window
import List exposing (..)
import Time exposing (..)
import String exposing (join)


-- MODEL

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
newBoard i =
  map (\row ->
        map
        (\col -> Open (row, col)
        ) [0..i]) [0..i]

gameMap : Int -> GameMap
gameMap i =
  let startLoc = (0,0) in
  { board = newBoard i
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

-- UPDATE

update : Input -> Game -> Game
update i game =
  { game | gameMap <- updateGameMap i game.gameMap }

updateGameMap : Input -> GameMap -> GameMap
updateGameMap {dir} ({board,start,currentPlayerLocation} as gameMap) =
  let
    newLocation = translate currentPlayerLocation dir
  in
    if within board newLocation
    then
      { gameMap | currentPlayerLocation <- newLocation
      }
    else
      gameMap

-- VIEW

view : Game -> Element
view g = leftAligned (Text.monospace (Text.fromString (toString g.gameMap)))

toString : GameMap -> String
toString {board,start,currentPlayerLocation} =
  let
      rowifier =
        (\row ->
          map
            (\cell ->
              if  | isAt currentPlayerLocation cell -> "@"
                  | isAt start cell -> "☐"
                  | otherwise -> "."
            )
            row
          |> join ""
        )
  in
    map rowifier board
      |> join "\n"

-- SIGNALS

main =
  Signal.map view gameState


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input


delta =
  Signal.map inSeconds (fps 35)


input : Signal Input
input =
  Signal.map Input Keyboard.wasd
