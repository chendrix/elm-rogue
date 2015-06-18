module Rogue.View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import String exposing (join)
import Text
import Array exposing (..)

import Rogue.Model exposing (..)

view : Game -> Element
view g = leftAligned (Text.monospace (Text.fromString (toString g.gameMap)))

toString : GameMap -> String
toString {board,start,currentPlayerLocation} =
  let
    rowifier =
      (\row ->
        Array.map
          (\cell ->
            if  | isAt currentPlayerLocation cell -> "@"
                | not <| isOpen cell -> "#"
                | otherwise -> "."
          )
          row
        |> toList
        |> join ""
      )
  in
    Array.map rowifier board
      |> toList
      |> join "\n"