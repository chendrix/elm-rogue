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
  }

type Item = Item

type alias Contents = 
  { player : Maybe Player 
  , items : List Item
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
  Open {player = Nothing, items = []}

defaultGameMap : Int -> GameMap
defaultGameMap size = Matrix.initialize size (always defaultCell)

generateMap : GameMap -> List (GameMap -> GameMap) -> GameMap
generateMap startingMap mapTransformers =
  foldr (<|) startingMap mapTransformers

defaultGame : Game
defaultGame =
  let
    p = newPlayer
    startLoc = (0,0)
    size = 10
    g = generateMap (defaultGameMap size)
          [ insertPlayer p startLoc
          , setBarriers (randomizeLocationsWithin size 11)
          , addItems (randomizeLocationsWithin size 30)
          ]
  in
    { gameMap = g
    , player = p
    }

updateContents : (Contents -> Contents) -> Cell -> Cell
updateContents f c =
  case c of
    Open contents -> Open (f contents)
    otherwise -> c

addPlayer : Player -> Cell -> Cell
addPlayer p c =
  let
    f : Contents -> Contents
    f = (\contents -> 
          { contents | 
              player <- Just 
                { p | inventory <- (p.inventory `append` contents.items) 
                },
              items <- [] 
          }
        )
  in updateContents f c
    

removePlayer : Cell -> Cell
removePlayer =
  updateContents (
    \contents -> {contents | player <- Nothing}
  )

insertPlayer : Player -> Location -> GameMap -> GameMap
insertPlayer p here gm =
  Matrix.mapWithLocation (
    \location cell -> if location == here then addPlayer p cell else cell
  ) gm

setBarriers : List Location -> GameMap -> GameMap
setBarriers barrierLocations gm =
  Matrix.mapWithLocation (
    \location cell -> if | doesContainPlayer cell -> cell
                         | location `member` barrierLocations -> Barrier {}
                         | otherwise -> cell
  ) gm

addItems : List Location -> GameMap -> GameMap
addItems itemLocations gm =
  let
    numItems = (\location -> filter (\otherLoc -> otherLoc == location) itemLocations |> length)
  in
    Matrix.mapWithLocation (
      \location cell -> if | location `member` itemLocations -> 
                              updateContents (\c -> { c | items <- (repeat (numItems location) Item) }) cell
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
