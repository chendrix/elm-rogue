module Rogue.View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import String exposing (join)
import Text
import Matrix exposing (..)

import List
import Rogue.Model exposing (..)

view : Game -> Element
view g =
  if | gameOver g -> txt "Game Over!"
     | otherwise -> flow down
        [ viewGameMap g.playerLocation g.gameMap
        , viewPlayer g.player
        ]

viewGameMap : Location -> GameMap -> Element
viewGameMap playerLocation gameMap =
  Matrix.mapWithLocation (viewCell playerLocation) gameMap
  |> Matrix.toList
  |> List.map (flow right)
  |> flow down

viewCell : Location -> Location -> Cell -> Element
viewCell curLocation cellLocation c =
  if | curLocation == cellLocation -> person
     | otherwise -> case c of
        Open contents -> open contents
        Barrier _ -> barrier

viewPlayer : Player -> Element
viewPlayer {inventory, hp} =
  flow down
    [ txt (String.join "" ["Item Count: ", List.length inventory |> toString ])
    , txt (String.join "" ["Current HP: ", floor hp |> toString ])
    ]


txt str =
  Text.fromString str
  |> Text.monospace
  |> centered

person =
  txt "@"
  |> standardize

barrier =
  square 16
  |> filled red
  |> (\sq -> collage 16 16 [sq])
  |> standardize

open : Contents -> Element
open {items} =
  let
    itemCount = List.length items
  in
    (if itemCount == 0 then "." else (itemCount |> toString))
    |> txt
    |> standardize

unoccupied =
  txt "."
  |> standardize

standardize : Element -> Element
standardize el =
  el
  |> container 16 16 middle
