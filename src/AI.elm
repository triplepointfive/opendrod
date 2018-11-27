module AI exposing (turn, squareDistanceToPlayer)

import Creature exposing (..)
import Room exposing (..)
import Utils exposing (..)

import Debug

turn : Creature -> Room -> Room
turn creature =
  case creature of
    Roach coord -> roach coord

roach : Coord -> Room -> Room
roach coord room =
  if List.member (Roach coord) room.creatures
    then
      let
        dx = sign <| (modBy room.width room.playerCoord) - (modBy room.width coord)
        dy = room.width * sign ((room.playerCoord // room.width) - (coord // room.width))
        directMoves =
          (dx + dy + coord) ::
          (List.sortBy (squareDistanceToPlayer room) [dy + coord, dx + coord])
        newCoord =
          case List.filter (canMoveTo coord room) directMoves of
            freeCoord :: _ -> freeCoord
            _ -> coord
      in
      { room
      | creatures = Roach newCoord :: List.filter ((/=) (Roach coord)) room.creatures
      }
    else
      room

squareDistanceToPlayer : Room -> Coord -> Int
squareDistanceToPlayer level coords =
  let
    (px, py) = toPoint level.width level.playerCoord
    (x, y) = toPoint level.width coords
    (dx, dy) = (px - x, py - y)
  in
    dx * dx + dy * dy

canMoveTo : Coord -> Room -> Coord -> Bool
canMoveTo prevCoord level coord =
  canRPlayerMoveTo prevCoord level coord && level.swordPos /= coord
