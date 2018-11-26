module Room exposing (..)

import Array
import Dict

import Dir
import Utils exposing (..)

type alias ObsticalId = Int

type alias RoomId = Point

type DoorState
  = Closed
  | Open

type OrbAction
  = Close
  | Toggle
  | ToOpen

type Tile
  = Wall
  | Floor
  | Orb (List (ObsticalId, OrbAction))
  | Checkpoint
  | Door ObsticalId DoorState
  | GreenDoor DoorState
  | Arrow Dir.Dir

type alias Creature = Coord

type alias Room =
  { blueprint : Array.Array Tile
  , creatures : List Creature
  , swordPos : Coord
  , playerCoord : Coord
  , playerDir : Dir.Dir
  , turn : Int
  , width : Int
  , height : Int
  , wallTiles : Array.Array (Int, Int)
  }

type MoveResult = Move Room | Leave Point Point

turn : Room -> Room
turn room = { room | turn = modBy 30 (room.turn + 1) }

movePlayer : Dir.Dir -> Room -> Room
movePlayer dir room = buildSwordPos { room | playerCoord = Dir.moveCoord room.width room.playerCoord dir }

mapRoomTiles : (Tile -> Tile) -> Room -> Room
mapRoomTiles f room = { room | blueprint = Array.map f room.blueprint }

openGreenDoor : Room -> Room
openGreenDoor = mapRoomTiles (\tile -> if tile == GreenDoor Closed then GreenDoor Open else tile)

turnSword : (Dir.Dir -> Dir.Dir) -> Room -> Room
turnSword f room = buildSwordPos { room | playerDir = f room.playerDir }

isOut : Point -> Room -> Bool
isOut (x, y) { width, height } = x < 0 || y < 0 || x >= width || y >= height

-- PRETTIFY
buildSwordPos : Room -> Room
buildSwordPos room =
  { room | swordPos = toCoord (either identity (const (-1, -1)) (\p -> isOut p room) (destSwordPos room)) room.width }

destSwordPos : Room -> Point
destSwordPos { width, playerCoord, playerDir } = Dir.move playerDir (toPoint width playerCoord)

  -- { room | swordPos =  }

isClear : Room -> Bool
isClear = List.isEmpty << .creatures

shiftPos : Point -> Room -> Point
shiftPos (x, y) { width, height, playerCoord } =
  let (px, py) = toPoint width playerCoord in (modBy width <| x + px, modBy height <| y + py)

canRPlayerMoveTo : Coord -> Room -> Coord -> Bool
canRPlayerMoveTo prevCoord level coord =
  let
    isUntaken = List.isEmpty <| List.filter ((==) coord) level.creatures
    dir = Dir.fromDelta (sub (toPoint level.width coord) (toPoint level.width prevCoord))

    canLeave = case Array.get prevCoord level.blueprint of
      Just (Arrow d) -> not (Dir.isOpposite d dir)
      _ -> True
  in
  canLeave && isUntaken && case Array.get coord level.blueprint of
    Nothing -> False
    Just Floor -> True
    Just Wall -> False
    Just (Orb _) -> False
    Just Checkpoint -> True
    Just (Door _ Closed) -> False
    Just (Door _ Open) -> True
    Just (GreenDoor Closed) -> False
    Just (GreenDoor Open) -> True
    Just (Arrow d) -> not (Dir.isOpposite d dir)

concatLevels : Room -> Room -> Point -> Room
concatLevels origin addend (dx, dy) =
  let size = 32 * 38 in
  if dy < 0 then
    { origin
    | blueprint = Array.append addend.blueprint origin.blueprint
    , height    = origin.height + origin.height
    , wallTiles = Array.append addend.wallTiles origin.wallTiles
    , creatures = addend.creatures ++ List.map ((+) size) origin.creatures

    , swordPos  = origin.swordPos + size
    , playerCoord = origin.playerCoord + size
    }
  else if dy > 0 then
    { origin
    | blueprint = Array.append origin.blueprint addend.blueprint
    , height    = origin.height + origin.height
    , wallTiles = Array.append origin.wallTiles addend.wallTiles
    , creatures = origin.creatures ++ List.map ((+) size) addend.creatures
    }
  else if dx > 0 then
    { origin
    | blueprint = appendV addend.blueprint origin.blueprint
    , width     = origin.width + origin.width
    , wallTiles = appendV addend.wallTiles origin.wallTiles
    -- , creatures = origin.creatures ++ List.map ((+) size) addend.creatures
    }
  else
    { origin
    | blueprint = appendV origin.blueprint addend.blueprint
    , width     = origin.width + origin.width
    , wallTiles = appendV origin.wallTiles addend.wallTiles
    -- , creatures = origin.creatures ++ List.map ((+) size) addend.creatures
    }

appendV : Array.Array a -> Array.Array a -> Array.Array a
appendV xs ys =
  let row i arr = Array.slice (i * 38) ((i + 1) * 38) arr
  in
  List.foldl (\x acc -> Array.append x acc) Array.empty
    <| List.concatMap (\i -> [row i xs, row i ys])
    <| List.reverse <| List.range 0 31
