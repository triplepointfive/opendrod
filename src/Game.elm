module Game exposing (..)

import Array

import AI
import Dir
import Level
import Room exposing (..)
import Utils exposing (..)

type alias Game =
  { room : Room
  , alive : Bool
  , backsteps : List Room
  , checkpoints : List Room
  , level : Level.Level
  }

onRoom : (Room -> Room) -> Game -> Game
onRoom f model = { model | room = f model.room }

-- postProcessTile : Model -> Model
-- postProcessTile model =
--   case Array.get model.currentRoom.playerCoord model.currentRoom.blueprint of
--     Just Checkpoint ->
--       { model
--       | checkpoints = model.currentRoom :: model.checkpoints
--       }
--
--     _ -> model

-- TODO: updateGame <<  add checkpoint if movement succeed
-- checkRoomStatus : Game -> Game
-- checkRoomStatus =

turn : (Dir.Dir -> Dir.Dir) -> Game -> Game
turn f = ifAlive (updateRoom) << ifAlive (aiTurn << triggerSword) << onRoom (turnSword f)

move : Dir.Dir -> Game -> Game
move dir = onRoom buildSwordPos


playerMoveDir : Dir.Dir -> Room -> MoveResult
playerMoveDir dir level =
  let
    destPos = Dir.moveCoord level.width level.playerCoord dir
    (px, py) = toCoords level.width level.playerCoord
    (x, y) = Dir.move dir (px, py)
    (lx, ly) = Dir.delta dir
  in
    if canPlayerMoveTo level.playerCoord level destPos
    then Move { level | playerCoord = destPos }
    else if x >= 0 && y >= 0 && x < level.width && y < level.height
      then Move level
      else
        let delta = if x < 0 || x >= level.width then (lx, 0) else (0, ly) in
        Leave
          delta

          ( modBy level.width (px + fst delta)
          , modBy level.height (py + snd delta)
          )








buildSwordPos : Room -> Room
buildSwordPos room = { room | swordPos = Dir.moveCoord room.width room.playerCoord room.playerDir }

swordToggle : Room -> Room
swordToggle room =
  case Array.get room.swordPos room.blueprint of
    Just (Orb actions) -> mapRoomTiles (orbAction actions) room
    _ -> room

orbAction : List (ObsticalId, OrbAction) -> Tile -> Tile
orbAction actions tile =
  case tile of
    Door id state ->
      case List.filter (\(i, _) -> i == id) actions of
        (_, action) :: _ ->
          Door id <| case action of
            Close -> Closed
            ToOpen -> Open
            Toggle -> if state == Closed then Open else Closed
        _ -> tile
    _ -> tile

turnSword : (Dir.Dir -> Dir.Dir) -> Room -> Room
turnSword f room = buildSwordPos { room | playerDir = f room.playerDir }

-- onMove : Game -> Game
-- onMove = ifAlive (updateRoom) << ifAlive (aiTurn << triggerSword) << updateAlive << move

triggerSword : Game -> Game
triggerSword = onRoom (swordToggle << swordKill)

swordKill : Room -> Room
swordKill room = { room | creatures = List.filter ((/=) room.swordPos) room.creatures }

updateRoom : Game -> Game
updateRoom = ifClear (onRoom (mapRoomTiles (\tile -> if tile == GreenDoor Closed then GreenDoor Open else tile)))

ifClear : (Game -> Game) -> Game -> Game
ifClear f game = if List.isEmpty game.room.creatures then f game else game

mapRoomTiles : (Tile -> Tile) -> Room -> Room
mapRoomTiles f room = { room | blueprint = Array.map f room.blueprint }

aiTurn : Game -> Game
aiTurn game = List.foldl creatureTurn game game.room.creatures

ifAlive : (Game -> Game) -> Game -> Game
ifAlive f game = if game.alive then f game else game

creatureTurn : Creature -> Game -> Game
creatureTurn creature = ifAlive (updateAlive << onRoom (AI.roach creature))

updateAlive : Game -> Game
updateAlive game = { game | alive = List.all ((/=) game.room.playerCoord) game.room.creatures }
