module Rogue.View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import String exposing (join)
import Text
import List exposing (..)

import Rogue.Model exposing (..)

view : Game -> Element
view g = leftAligned (Text.monospace (Text.fromString (toString g.gameMap)))

toString : GameMap -> String
toString {board,start,currentPlayerLocation} =
  let
      rowifier =
        (\row ->
          map
            (\cell ->
              if  | isAt currentPlayerLocation cell -> "@"
                  | isAt start cell -> "☐"
                  | otherwise -> "."
            )
            row
          |> join ""
        )
  in
    map rowifier board
      |> join "\n"