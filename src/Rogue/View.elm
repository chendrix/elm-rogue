module Rogue.View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import String exposing (join)
import Text
import Array exposing (..)

import Rogue.Model exposing (..)

view : Game -> Element
view g = viewGameMap g.gameMap

viewGameMap : GameMap -> Element
viewGameMap gameMap =
  let
    rowifier =
      (\row ->
        Array.map viewCell row
        |> toList
        |> flow right
      )
  in
    Array.map rowifier gameMap
      |> toList
      |> flow down

viewCell : Cell -> Element
viewCell c =
  case c of
    Open {player} -> open player
    Barrier _ -> barrier

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

open : Maybe a -> Element
open p =
  case p of
    Just _ -> person
    Nothing -> unoccupied
  
unoccupied =
  txt "."
  |> standardize

standardize : Element -> Element
standardize el =
  el
  |> container 16 16 middle
