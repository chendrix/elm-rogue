module RogueTest where

import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)

import Rogue.Model exposing (..)
import Rogue.Update exposing (..)
import String

tests : Test
tests = suite "A Test Suite"
  [ test_updateGameMap_does_not_move_outside_of_bounds
  
  ]

test_updateGameMap_does_not_move_outside_of_bounds : Test
test_updateGameMap_does_not_move_outside_of_bounds = 
  let 
    game = gameMap 2
    gameAtTopLeft = { game | currentPlayerLocation <- (0,0) }
    left = Input { x =-1, y = 0 }
    up = Input { x=0, y=1 }
    gameAtBottomRight = { game | currentPlayerLocation <- (1,1) }
    right = Input { x =1, y = 0 }
    down = Input { x=0, y=-1 }
  in
    suite "Update does not move outside of bounds"
    [ test "Left" (
        assertEqual gameAtTopLeft.currentPlayerLocation (updateGameMap left gameAtTopLeft).currentPlayerLocation
      )
    , test "Up" (
        assertEqual gameAtTopLeft.currentPlayerLocation (updateGameMap up gameAtTopLeft).currentPlayerLocation
      )
    , test "Right" (
        assertEqual gameAtBottomRight.currentPlayerLocation (updateGameMap right gameAtBottomRight).currentPlayerLocation
      )
    , test "Down" (
        assertEqual gameAtBottomRight.currentPlayerLocation (updateGameMap down gameAtBottomRight).currentPlayerLocation
      )
    ]