module Rogue.Update where

import Rogue.Model exposing (..)

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