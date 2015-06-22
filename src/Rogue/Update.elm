module Rogue.Update where

import Rogue.Model exposing (..)
import Matrix exposing (..)
import Maybe
import Maybe exposing (andThen)
import List exposing (append)

update : Input -> Game -> Game
update {dir} ({gameMap,player,playerLocation} as game) =
  let
    translatedLocation = (translate dir playerLocation)
    newLocation = movePlayerToLocation gameMap playerLocation translatedLocation

    newCell = cellAt gameMap newLocation
    newPlayer = Maybe.map (flip updatePlayer <| player) newCell |> Maybe.withDefault player

    newMap = updateGameMap newLocation gameMap
  in
    { game  | gameMap <- newMap
            , player <- newPlayer
            , playerLocation <- newLocation
    }

updatePlayer : Cell -> Player -> Player
updatePlayer cell player =
  case cell of
    Open {items} -> {player | inventory <- player.inventory `append` items}
    otherwise -> player

updateGameMap : Location -> GameMap -> GameMap
updateGameMap newPlayerLoc gameMap =
  mapWithLocation (
    \location cell ->
      if  | location == newPlayerLoc -> removeItems cell
          | otherwise -> cell
  ) gameMap

removeItems : Cell -> Cell
removeItems =
  updateContents (\contents -> {contents | items <- [] })


movePlayerToLocation : GameMap -> Location -> Location -> Location
movePlayerToLocation gameMap fromLoc toLoc =
  let
    cellChooser cell =
      case cell of
        Open _ -> toLoc
        otherwise -> fromLoc
  in
    cellAt gameMap toLoc
    |> Maybe.map cellChooser
    |> Maybe.withDefault fromLoc
