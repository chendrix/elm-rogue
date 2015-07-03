module Rogue where

import Keyboard
import Numpad

import Rogue.Model exposing (..)
import Rogue.Update exposing (..)
import Rogue.View exposing (..)
import Signal exposing (..)
import Time exposing (..)

-- SIGNALS

main =
  Signal.map view gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

input : Signal Input
input =
  mergeMany
    [ (\d -> Input { dir = d }) <~
        Signal.mergeMany
          [ Keyboard.wasd
          , Keyboard.arrows
          , Numpad.numpad
          ]
    , TimeDelta <~ delta
    ]

delta : Signal Time
delta = Signal.map (\t -> t / 20) (fps 30)