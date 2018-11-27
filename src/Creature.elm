module Creature exposing (..)

import Utils exposing (..)

type LarvaStep = L1 | L2 | L3 | L4

type Creature
  = Roach Coord
  | Larva Coord LarvaStep
  | Queen Coord

isTaken : Coord -> Creature -> Bool
isTaken c creature =
  case creature of
    Roach coord -> coord == c
    Larva coord _ -> coord == c
    Queen coord -> coord == c

move : (Coord -> Coord) -> Creature -> Creature
move f creature =
  case creature of
    Roach coord -> Roach (f coord)
    Larva coord step -> Larva (f coord) step
    Queen coord -> Queen (f coord)
