module Dir exposing (..)

import Utils exposing (..)

type Dir = N | NE | E | SE | S | SW | W | NW

all : List Dir
all = [N, NE, E, SE, S, SW, W, NW]

moveCoord : Int -> Coord -> Dir -> Coord
moveCoord w coord dir = let (dx, dy) = delta dir in coord + dx + w * dy

move : Dir -> Point -> Point
move dir = add (delta dir)

delta : Dir -> Point
delta dir =
  case dir of
    N  -> (0, -1)
    NE -> (1, -1)
    E  -> (1, 0)
    SE -> (1, 1)
    S  -> (0, 1)
    SW -> (-1, 1)
    W  -> (-1, 0)
    NW -> (-1, -1)

fromDelta : Point -> Dir
fromDelta (dx, dy) =
  if dx > 0 && dy > 0 then SE
  else if dx > 0 && dy < 0 then NE
  else if dx > 0 then E
  else if dx < 0 && dy > 0 then SW
  else if dx < 0 && dy < 0 then NW
  else if dx < 0 then W
  else if dy > 0 then S
  else N -- dy < 0

left : Dir -> Dir
left dir =
  case dir of
    N  -> NW
    NE -> N
    E  -> NE
    SE -> E
    S  -> SE
    SW -> S
    W  -> SW
    NW -> W

right : Dir -> Dir
right dir =
  case dir of
    N  -> NE
    NE -> E
    E  -> SE
    SE -> S
    S  -> SW
    SW -> W
    W  -> NW
    NW -> N

isOpposite : Dir -> Dir -> Bool
isOpposite d1 d2 =
  case d1 of
    N -> List.member d2 [SW, S, SE]
    S -> List.member d2 [NW, N, NE]
    W -> List.member d2 [NE, E, SE]
    E -> List.member d2 [NW, W, SW]
    NE -> List.member d2 [W, S, SW]
    NW -> List.member d2 [E, S, SE]
    SE -> List.member d2 [W, N, NW]
    SW -> List.member d2 [E, N, NE]
