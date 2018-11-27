module Creature exposing (..)

import Utils exposing (..)

type Creature
  = Roach Coord

isTaken : Coord -> Creature -> Bool
isTaken c creature =
  case creature of
    Roach coord -> coord == c

move : (Coord -> Coord) -> Creature -> Creature
move f creature =
  case creature of
    Roach coord -> Roach (f coord)
