module Rogue.Model where

import List exposing (..)
import Maybe exposing (..)
import Random exposing (..)
import Matrix exposing (..)

import Now

type alias GameMap = Matrix Cell

type alias Player =
  { inventory : List Item
  }

type alias Game =
  { gameMap : GameMap
  , player : Player
  , playerLocation : Location
  }

type Item = Item

type alias Contents =
  { items : List Item
  }

type Cell
  = Open Contents
  | Barrier {}

type alias Dir =
  { x : Int
  , y : Int
  }

type alias Input =
  { dir : Dir
  }

newPlayer : Player
newPlayer = {inventory = []}

randomizeLocationsWithin : Int -> Int -> List Location
randomizeLocationsWithin size numLocations =
  let
    locationGenerator = list numLocations (pair (int 0 size) (int 0 size))
  in
    generate locationGenerator (initialSeed (round Now.loadTime)) |> fst

defaultCell : Cell
defaultCell =
  Open {items = []}

defaultGameMap : Int -> GameMap
defaultGameMap size = Matrix.initialize size (always defaultCell)

generateMap : GameMap -> List (GameMap -> GameMap) -> GameMap
generateMap startingMap mapTransformers =
  foldr (<|) startingMap mapTransformers

generateGame : Game -> List (Game -> Game) -> Game
generateGame =
  foldr (<|)

defaultGame : Game
defaultGame =
  let
    p = newPlayer
    startLoc = (0,0)
    size = 10
    g = generateMap (defaultGameMap size) []
  in
    { gameMap = g
    , player = p
    , playerLocation = startLoc
    }

newGame : Game
newGame =
  let
    p = newPlayer
    startLoc = (0,0)
    size = 10
    g = generateMap (defaultGameMap size) []
    startingGame =  { gameMap = g
                    , player = p
                    , playerLocation = startLoc
                    }
  in
    generateGame startingGame
      [ setBarriers (randomizeLocationsWithin size 11)
      , addItems (randomizeLocationsWithin size 30)
      ]

updateContents : (Contents -> Contents) -> Cell -> Cell
updateContents f c =
  case c of
    Open contents -> Open (f contents)
    otherwise -> c

setBarriers : List Location -> Game -> Game
setBarriers barrierLocations game =
  let
    newMap = Matrix.mapWithLocation (
      \location cell -> if | location == game.playerLocation -> cell
                           | location `member` barrierLocations -> Barrier {}
                           | otherwise -> cell
    ) game.gameMap
  in
    { game | gameMap <- newMap }

addItems : List Location -> Game -> Game
addItems itemLocations game =
  let
    numItems location = length <| filter ((==) location) itemLocations
    newMap =
      Matrix.mapWithLocation (
        \location cell -> if | location `member` itemLocations ->
                                updateContents (\c -> { c | items <- (repeat (numItems location) Item) }) cell
                             | otherwise -> cell

      ) game.gameMap
  in
    { game | gameMap <- newMap }


isJust : Maybe a -> Bool
isJust m =
  case m of
    Just _ -> True
    Nothing -> False

translate : Dir -> Location -> Location
translate  dir (row,col)=
  (row - dir.y, col + dir.x)
