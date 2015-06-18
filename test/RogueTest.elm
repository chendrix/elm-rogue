module RogueTest where

import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)

import Rogue.Model exposing (..)
import Rogue.Update exposing (..)
import String



tests : Test
tests = suite "A Test Suite"
  [ test_updateGameMap_does_not_move_outside_of_bounds
  , test_update_moves_currentPlayerLocation
  , test_updateGameMap_does_not_move_into_barriers
  ]

test_update_moves_currentPlayerLocation : Test
test_update_moves_currentPlayerLocation =
  let
    defaultGameMap = gameMap 2
    game_map = { defaultGameMap | currentPlayerLocation <- (0,0) }
    player = Player
    game = Game game_map player
    down_right = Input {x=1,y=-1}
  in
    test "Update moves currentPlayerLocation" (
        assertEqual (update down_right game).gameMap.currentPlayerLocation (1,1)
      )

test_updateGameMap_does_not_move_outside_of_bounds : Test
test_updateGameMap_does_not_move_outside_of_bounds = 
  let 
    game_map = gameMap 2
    gameMapAtTopLeft = { game_map | currentPlayerLocation <- (0,0) }
    left = Input { x =-1, y = 0 }
    up = Input { x=0, y=1 }
    gameMapAtBottomRight = { game_map | currentPlayerLocation <- (1,1) }
    right = Input { x =1, y = 0 }
    down = Input { x=0, y=-1 }
  in
    suite "Update does not move outside of bounds"
    [ test "Left" (
        assertEqual gameMapAtTopLeft.currentPlayerLocation (updateGameMap left gameMapAtTopLeft).currentPlayerLocation
      )
    , test "Up" (
        assertEqual gameMapAtTopLeft.currentPlayerLocation (updateGameMap up gameMapAtTopLeft).currentPlayerLocation
      )
    , test "Right" (
        assertEqual gameMapAtBottomRight.currentPlayerLocation (updateGameMap right gameMapAtBottomRight).currentPlayerLocation
      )
    , test "Down" (
        assertEqual gameMapAtBottomRight.currentPlayerLocation (updateGameMap down gameMapAtBottomRight).currentPlayerLocation
      )
    ]

test_updateGameMap_does_not_move_into_barriers : Test
test_updateGameMap_does_not_move_into_barriers =
  let 
    board = newBoardWithBarriersAt 2 [(0,1)]
    game_map = GameMap board (0,0) (0,0)
    right = Input { x =1, y = 0 }
  in
    test "Update does not move into barriers" (
        assertEqual game_map.currentPlayerLocation (updateGameMap right game_map).currentPlayerLocation
      )
    