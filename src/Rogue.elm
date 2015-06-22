module Rogue where

import Keyboard
import Numpad

import Rogue.Model exposing (..)
import Rogue.Update exposing (..)
import Rogue.View exposing (..)

-- SIGNALS

main =
  Signal.map view gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

input : Signal Input
input =
  Signal.map Input <| Signal.mergeMany
    [ Keyboard.wasd
    , Keyboard.arrows
    , Numpad.numpad
    ]
