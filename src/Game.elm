module Game exposing (Game, move, turn)

import Array
import Dict
import Maybe

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
turn f = afterPlayer << onRoom (turnSword f)

afterPlayer : Game -> Game
afterPlayer = ifAlive (updateRoom) << ifAlive (aiTurn << triggerSword) << onRoom (buildSwordPos)

chain : (a -> Maybe a) -> a -> a -> a
chain f i a = Maybe.withDefault a (f i)

move : Dir.Dir -> Game -> Game
move dir game = chain (leave dir) game <| chain (Maybe.map afterPlayer << movePlayer dir) game <| failMove game

leave : Dir.Dir -> Game -> Maybe Game
leave dir = const Nothing

movePlayer : Dir.Dir -> Game -> Maybe Game
movePlayer dir = canDo (moveTo dir) (onRoom (moveRPlayer dir))

-- TODO : Simplify
moveTo : Dir.Dir -> Game -> Bool
moveTo dir { room } =
  canRPlayerMoveTo room.playerCoord room (Dir.moveCoord room.width room.playerCoord dir)

-- TODO : Extract to Room & rename
moveRPlayer : Dir.Dir -> Room -> Room
moveRPlayer dir room = { room | playerCoord = Dir.moveCoord room.width room.playerCoord dir }

failMove : Game -> Game
failMove = afterPlayer


  -- let
  --   dest = destPos dir game.room
  --   adj = add (roomDelta dest game.room) game.level.currentRoomId
  -- in
  -- if outOfRoom dest game.room && canLeave adj game
  --   then leaveRoom game adj
  --   else moveTo dest

outOfRoom : Point -> Room -> Bool
outOfRoom (x, y) { width, height } = x < 0 || y < 0 || x >= width || y >= height

canLeave : Point -> Game -> Bool
canLeave dest { level } = Dict.member dest level.rooms

roomDelta : Point -> Room -> Point
roomDelta (x, y) { width } = if x < 0 then (-1, 0) else if y < 0 then (0, -1) else if x >= width then (1, 0) else (0, 1)

--   Leave delta newPlayerPos ->
--     let newRoomId = add model.level.currentRoomId delta in
--     case Dict.get newRoomId model.level.rooms of
--       Just { builder, state } ->
--         let
--           nextLevel =
--             postProcessRoom <|
--             Level.buildRoom
--               (state == Level.Complete)
--               newPlayerPos
--               model.currentRoom.playerDir
--               builder
--         in
--         { model
--         | effect = Just <| ChangeRoom nextLevel 0 delta
--         , currentRoom = concatLevels model.currentRoom nextLevel delta
--         , level = Level.move (List.isEmpty model.currentRoom.creatures) newRoomId model.level
--         }
--     (lx, ly) = Dir.delta dir
--
--   let delta = if x < 0 || x >= level.width then (lx, 0) else (0, ly) in
--            delta
--
--            ( modBy level.width (px + fst delta)
--            , modBy level.height (py + snd delta)
--            )

destPos : Dir.Dir -> Room -> Point
destPos dir { width, playerCoord } = add (Dir.delta dir) (toCoords width playerCoord)

-- afterMove : Dir.Dir -> (Game -> Game) -> (Game -> Game) -> Game -> Game
-- afterMove dir success fail game =
--   if canPlayerMoveTo game.room.playerCoord game.room
--     then success <| onRoom (buildSwordPos << movePlayer dir) game
--     else fail game
--
-- movePlayer : Dir.Dir -> Room -> Room
-- movePlayer dir room = { room | playerCoord = Dir.moveCoord room.width room.playerCoord dir }
-- TODO: Remove duplicity

-- withMAction : (Room -> MoveResult) -> Model -> Model
-- withMAction action model = model
  -- case action model.currentRoom of
  --   Move room -> withAction (const room) model
  --   Leave delta newPlayerPos ->
  --     let newRoomId = add model.level.currentRoomId delta in
  --     case Dict.get newRoomId model.level.rooms of
  --       Just { builder, state } ->
  --         let
  --           nextLevel =
  --             postProcessRoom <|
  --             Level.buildRoom
  --               (state == Level.Complete)
  --               newPlayerPos
  --               model.currentRoom.playerDir
  --               builder
  --         in
  --         { model
  --         | effect = Just <| ChangeRoom nextLevel 0 delta
  --         , currentRoom = concatLevels model.currentRoom nextLevel delta
  --         , level = Level.move (List.isEmpty model.currentRoom.creatures) newRoomId model.level
  --         }
  --       Nothing -> withAction (const model.currentRoom) model


-- playerMoveDir : Dir.Dir -> Room -> MoveResult
-- playerMoveDir dir level =
--   let
--     destPos = Dir.moveCoord level.width level.playerCoord dir
--     (px, py) = toCoords level.width level.playerCoord
--     (x, y) = Dir.move dir (px, py)
--     (lx, ly) = Dir.delta dir
--   in
--     if canPlayerMoveTo level.playerCoord level destPos
--     then Move { level | playerCoord = destPos }
--     else if x >= 0 && y >= 0 && x < level.width && y < level.height
--       then Move level
--       else
--         let delta = if x < 0 || x >= level.width then (lx, 0) else (0, ly) in
--         Leave
--           delta
--
--           ( modBy level.width (px + fst delta)
--           , modBy level.height (py + snd delta)
--           )








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
turnSword f room = { room | playerDir = f room.playerDir }

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
