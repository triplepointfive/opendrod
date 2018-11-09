module AI exposing (..)

import Level exposing (..)
import Utils exposing (..)

import Debug

roachAI : Creature -> Level -> Level
roachAI coord level =
  if List.member coord level.creatures
    then
      let dx = sign <| (modBy level.width level.playerCoord) - (modBy level.width coord)
          dy = level.width * sign ((level.playerCoord // level.width) - (coord // level.width))
          directMoves =
            (++)
              [dx + dy + coord]
              (List.sortBy (squareDistanceToPlayer level) [dy + coord, dx + coord])
          newCoord =
            case List.filter (\ i -> canMoveTo i level) directMoves of
              freeCoord :: _ -> freeCoord
              _ -> coord
      in
      { level
      | creatures = newCoord :: List.filter ((/=) coord) level.creatures
      }
    else
      level

squareDistanceToPlayer : Level -> Creature -> Int
squareDistanceToPlayer level coords =
  let
    (px, py) = toCoords level.width level.playerCoord
    (x, y) = toCoords level.width coords
    (dx, dy) = (px - x, py - y)
  in
    dx * dx + dy * dy
