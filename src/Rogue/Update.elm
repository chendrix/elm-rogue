module Rogue.Update where

import Rogue.Model exposing (..)
import Maybe

update : Input -> Game -> Game
update i game =
  { game | gameMap <- updateGameMap i game.gameMap }

updateGameMap : Input -> GameMap -> GameMap
updateGameMap {dir} ({board,start,currentPlayerLocation} as gameMap) =
  let
    newLocation = translate currentPlayerLocation dir
    newCell = cellAt newLocation board
    openness = Maybe.withDefault False (Maybe.map isOpen newCell)
  in
    if  | within board newLocation && openness -> { gameMap | currentPlayerLocation <- newLocation }
        | otherwise -> gameMap