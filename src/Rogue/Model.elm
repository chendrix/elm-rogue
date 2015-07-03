module Rogue.Model where

import List exposing (..)
import Maybe exposing (..)
import Random exposing (..)
import Matrix exposing (..)
import Time exposing (..)

import Now

type alias GameMap = Matrix Cell

type alias HitPoints = Float
type alias Player =
  { inventory : List Item
  , hp : HitPoints
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

gameOver : Game -> Bool
gameOver {player} = player.hp <= 0

newPlayer : Player
newPlayer = {inventory = [], hp = 100}

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

applyTransformations : a -> List (a -> a) -> a
applyTransformations =
  foldr (<|)

defaultGame : Game
defaultGame =
  newGame newPlayer (0,0) 10
    [ setBarriers (randomizeLocationsWithin 10 11)
    , addItems (randomizeLocationsWithin 10 30)
    ]

newGame : Player -> Location -> Int -> List (Game -> Game) -> Game
newGame p startLoc size transformations =
  let
    g = defaultGameMap size
    startingGame =  { gameMap = g
                    , player = p
                    , playerLocation = startLoc
                    }
  in
    applyTransformations startingGame transformations

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
