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
    player = newPlayer
    game_map = generateMap (defaultGameMap 2) [ insertPlayer player (0,0) ]
    down_right = Input {x=1,y=-1}
  in
    test "Update moves currentPlayerLocation" (
        assertEqual ((updateGameMap down_right player game_map) |> currentPlayerLocation) (Just (1,1))
      )

test_updateGameMap_does_not_move_outside_of_bounds : Test
test_updateGameMap_does_not_move_outside_of_bounds = 
  let 
    p = newPlayer
    game_map = defaultGameMap 2
    gameMapAtTopLeft = generateMap game_map [ insertPlayer p (0,0) ]
    left = Input { x =-1, y = 0 }
    up = Input { x=0, y=1 }
    gameMapAtBottomRight = generateMap game_map [ insertPlayer p (1,1) ]
    right = Input { x =1, y = 0 }
    down = Input { x=0, y=-1 }
  in
    suite "Update does not move outside of bounds"
    [ test "Left" (
        assertEqual (gameMapAtTopLeft |> currentPlayerLocation) ((updateGameMap left p gameMapAtTopLeft) |> currentPlayerLocation)
      )
    , test "Up" (
        assertEqual (gameMapAtTopLeft |> currentPlayerLocation) ((updateGameMap up p gameMapAtTopLeft) |> currentPlayerLocation)
      )
    , test "Right" (
        assertEqual (gameMapAtBottomRight |> currentPlayerLocation) ((updateGameMap right p gameMapAtBottomRight) |> currentPlayerLocation)
      )
    , test "Down" (
        assertEqual (gameMapAtBottomRight |> currentPlayerLocation) ((updateGameMap down p gameMapAtBottomRight) |> currentPlayerLocation)
      )
    ]

test_updateGameMap_does_not_move_into_barriers : Test
test_updateGameMap_does_not_move_into_barriers =
  let 
    p = newPlayer
    game_map = generateMap (defaultGameMap 2) [ insertPlayer p (0,0), setBarriers [(0,1)] ]
    right = Input { x =1, y = 0 }
  in
    test "Update does not move into barriers" (
        assertEqual (game_map |> currentPlayerLocation) ((updateGameMap right p game_map) |> currentPlayerLocation)
      )
    