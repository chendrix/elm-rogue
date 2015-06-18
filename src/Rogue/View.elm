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
viewGameMap {board,start,currentPlayerLocation} =
  let
    rowifier =
      (\row ->
        Array.map
          (\cell ->
            if  | isAt currentPlayerLocation cell -> person
                | not <| isOpen cell -> barrier
                | otherwise -> open
          )
          row
        |> toList
        |> flow right
      )
  in
    Array.map rowifier board
      |> toList
      |> flow down


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

open =
  txt "."
  |> standardize

standardize : Element -> Element
standardize el =
  el
  |> container 16 16 middle

--toString  =
--  let
--    rowifier =
--      (\row ->
--        Array.map
--          (\cell ->
--            if  | isAt currentPlayerLocation cell -> "@"
--                | not <| isOpen cell -> "#"
--                | otherwise -> "."
--          )
--          row
--        |> toList
--        |> join ""
--      )
--  in
--    Array.map rowifier board
--      |> toList
--      |> join "\n"