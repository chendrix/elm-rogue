module Rogue where

import Keyboard
import Numpad

import Rogue.Model exposing (..)
import Rogue.Update exposing (..)
import Rogue.View exposing (..)
import Signal exposing ((<~), (~), Signal)

-- SIGNALS

main =
  Signal.map view gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

input : Signal Input
input =
  Input <~
    Signal.mergeMany
      [ Keyboard.wasd
      , Keyboard.arrows
      , Numpad.numpad
      ]
