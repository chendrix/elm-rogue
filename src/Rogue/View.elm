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
view g = viewGameMap g.gameMap

viewGameMap : GameMap -> Element
viewGameMap gameMap =
  Matrix.map viewCell gameMap
  |> Matrix.toList
  |> List.map (flow right)
  |> flow down

viewCell : Cell -> Element
viewCell c =
  case c of
    Open contents -> open contents
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

open : Contents -> Element
open {player,items} =
  let 
    itemCount = List.length items 
  in
    case player of
      Just _ -> person
      Nothing -> 
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
