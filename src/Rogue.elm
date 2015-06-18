module Rogue where

import Keyboard
import Time exposing (..)

import Rogue.Model exposing (..)
import Rogue.Update exposing (..)
import Rogue.View exposing (..)

-- SIGNALS

main =
  Signal.map view gameState


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input


delta =
  Signal.map inSeconds (fps 35)


input : Signal Input
input =
  Signal.map Input Keyboard.wasd
