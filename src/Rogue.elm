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
  }

type alias Board = List (List Cell)

type Player = Player

type alias Game =
  { gameMap : GameMap
  , player : Player
  , location : Location
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
  { board = newBoard i
  , start = (0, 0)
  }

defaultGame : Game
defaultGame =
  let
    g = gameMap 10
  in
    { gameMap = g
    , player = Player
    , location = g.start
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
update {dir} ({gameMap,player,location} as game) =
  let
    (row, col) = location
    newLocation = translate location dir
  in
    if within gameMap.board newLocation
    then
      { game | location <- newLocation
      }
    else
      game

-- VIEW

view : Game -> Element
view g = leftAligned (Text.monospace (Text.fromString (toString g)))

toString : Game -> String
toString {gameMap,player,location} =
  let
    rowifier =
      (\row ->
        map
          (\cell ->
            if isAt location cell
            then "@"
            else "."
          )
          row
        |> join ""
      )
  in
    map rowifier gameMap.board
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
