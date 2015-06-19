module Rogue.Update where

import Rogue.Model exposing (..)
import Array
import Maybe
import Maybe exposing (andThen)

update : Input -> Game -> Game
update i game =
  { game | gameMap <- updateGameMap i game.player game.gameMap }

updateGameMap : Input -> Player -> GameMap -> GameMap
updateGameMap {dir} p gameMap =
  let
    maybeCurLocation = currentPlayerLocation gameMap
    maybeTranslatedLocation = Maybe.map (translate dir) (maybeCurLocation)
    maybeNewLocation = movePlayerToLocation gameMap maybeCurLocation maybeTranslatedLocation
    maybeNewGameMap = Maybe.map (updateBoard p gameMap) maybeNewLocation 
  in
    Maybe.withDefault gameMap maybeNewGameMap

updateBoard : Player -> GameMap -> Location -> GameMap
updateBoard p gameMap newPlayerLoc =
  Array.indexedMap (
    \rowNum row -> Array.indexedMap (
      \colNum cell -> 
        if  | (rowNum, colNum) == newPlayerLoc -> insertPerson p cell
            | otherwise -> clearCell cell
        ) row
  ) gameMap

insertPerson : Player -> Cell -> Cell
insertPerson p c =
  case c of
    Open _ -> Open {player = Just p}
    otherwise -> c

clearCell : Cell -> Cell
clearCell c =
  case c of
    Open _ -> Open {player = Nothing}
    otherwise -> c

movePlayerToLocation : GameMap -> Maybe Location -> Maybe Location -> Maybe Location
movePlayerToLocation gameMap fromLoc toLoc =
  toLoc `andThen` (cellAt gameMap) `andThen` (\cell -> 
      case cell of
        Open _ -> toLoc
        otherwise -> fromLoc
    )
