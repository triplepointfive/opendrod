module AI exposing (turn, squareDistanceToPlayer)

import Dir
import Creature exposing (..)
import Room exposing (..)
import Utils exposing (..)

import Array
import Debug

turn : Creature -> Room -> Room
turn creature =
  case creature of
    Roach coord -> roach coord
    Larva coord step -> larva coord step
    Queen coord -> queen coord

roach : Coord -> Room -> Room
roach coord room =
  if List.member (Roach coord) room.creatures
    then
      let
        dx = sign <| (modBy room.width room.playerCoord) - (modBy room.width coord)
        dy = room.width * sign ((room.playerCoord // room.width) - (coord // room.width))
        directMoves =
          (dx + dy + coord) ::
          (List.sortBy (coordSquareDistanceToPlayer room) [dy + coord, dx + coord])
        newCoord =
          case List.filter (canMoveTo coord room) directMoves of
            freeCoord :: _ -> freeCoord
            _ -> coord
      in
      replaceWith (Roach coord) (Roach newCoord) room
    else
      room

larva : Coord -> LarvaStep -> Room -> Room
larva c step = replaceWith (Larva c step) <|
  case step of
    L1 -> Larva c L2
    L2 -> Larva c L3
    L3 -> Larva c L4
    L4 -> Roach c

queen : Coord -> Room -> Room
queen coord room =
  if List.member (Queen coord) room.creatures
    then
      if room.turn == 29
        then
          { room
          | creatures =
              room.creatures
                ++ List.map (flip Larva L1) (List.filter (availableToSpawn room) <| coordsAround coord room)
          }
        else
          let
            dx = -1 * sign((modBy room.width room.playerCoord) - (modBy room.width coord))
            dy = -1 * room.width * sign ((room.playerCoord // room.width) - (coord // room.width))
            directMoves =
              (dx + dy + coord) ::
              (List.sortBy (coordSquareDistanceToPlayer room) [dy + coord, dx + coord])
              -- TODO : Recheck priority
            newCoord =
              case List.filter (canMoveTo coord room) directMoves of
                freeCoord :: _ -> freeCoord
                _ -> coord
            in
            replaceWith (Queen coord) (Queen newCoord) room
    else
      room

coordsAround : Coord -> Room -> List Coord
coordsAround coord room =
  List.map (flip toCoord room.width)
    <| List.filter (not << flip Room.isOut room)
    <| List.map (flip Dir.move (toPoint room.width coord)) Dir.all

replaceWith : Creature -> Creature -> Room -> Room
replaceWith old new room =
  { room
  | creatures = new :: List.filter((/=) old) room.creatures
  }

squareDistanceToPlayer : Room -> Creature -> Int
squareDistanceToPlayer room creature = coordSquareDistanceToPlayer room <|
  case creature of
    Roach coord -> coord
    Larva coord _ -> coord
    Queen coord -> coord

coordSquareDistanceToPlayer : Room -> Coord -> Int
coordSquareDistanceToPlayer { width, playerCoord } coords =
  let
    (dx, dy) = sub (toPoint width playerCoord) (toPoint width coords)
  in
    dx * dx + dy * dy

canMoveTo : Coord -> Room -> Coord -> Bool
canMoveTo prevCoord room coord =
  canRPlayerMoveTo prevCoord room coord && room.swordPos /= coord

-- TODO: Do not spawn under the player or sword
availableToSpawn : Room -> Coord -> Bool
availableToSpawn { creatures, blueprint } coord =
  List.all (not << Creature.isTaken coord) creatures && case Array.get coord blueprint of
    Nothing -> False
    Just Floor -> True
    Just Wall -> False
    Just (Orb _) -> False
    Just Checkpoint -> True
    Just (Door _ _) -> False
    Just (GreenDoor _) -> False
    Just (Arrow _) -> True

