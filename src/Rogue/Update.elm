module Rogue.Update where

import Rogue.Model exposing (..)
import Matrix exposing (..)
import Maybe
import Maybe exposing (andThen)
import List exposing (append)
import Time exposing (..)

type Input
  = Input { dir : Dir
          }
  | TimeDelta Time

update : Input -> Game -> Game
update input ({gameMap,player,playerLocation} as game) =
  case input of
    Input {dir} ->
      let
        translatedLocation = (translate dir playerLocation)
        newLocation = movePlayerToLocation gameMap playerLocation translatedLocation

        newCell = cellAt gameMap newLocation
        newPlayer =
          Maybe.map (flip updatePlayer <| player) newCell
          |> Maybe.withDefault player

        newMap = updateGameMap newLocation gameMap
      in
        { game  | gameMap <- newMap
                , player <- newPlayer
                , playerLocation <- newLocation
        }
    TimeDelta delta ->
      let
        newPlayer = { player | hp <- player.hp - (delta / 100) }
      in
        { game | player <- newPlayer }

updatePlayer : Cell -> Player -> Player
updatePlayer cell player =
  case cell of
    Open {items} -> { player
                        | inventory <- player.inventory `append` items
                        , hp <- player.hp + toFloat (List.length items)
                    }
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
