module Rogue.Update where

import Rogue.Model exposing (..)
import Matrix exposing (..)
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
  mapWithLocation (
      \(rowNum,colNum) cell -> 
        if  | (rowNum, colNum) == newPlayerLoc -> addPlayer p cell
            | otherwise -> removePlayer cell
    ) gameMap


movePlayerToLocation : GameMap -> Maybe Location -> Maybe Location -> Maybe Location
movePlayerToLocation gameMap fromLoc toLoc =
  toLoc `andThen` (cellAt gameMap) `andThen` (\cell -> 
      case cell of
        Open _ -> toLoc
        otherwise -> fromLoc
    )
