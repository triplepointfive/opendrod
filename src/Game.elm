module Game exposing (Game, move, turn, undo)

import Array
import Dict
import Maybe
import Debug

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

undo : Game -> Game
undo game =
  case game.backsteps of
    x :: xs ->
      { game
      | room = x
      , backsteps = xs
      , alive = True -- TODO: Consider moving to currentRoom
      }
    _ -> game

turn : (Dir.Dir -> Dir.Dir) -> Game -> Game
turn f = afterAction << onRoom (Room.turnSword f) << saveBackstep

move : Dir.Dir -> Game -> Game
move dir = ifAlive (beforeAction >> moveAction dir)

beforeAction : Game -> Game
beforeAction = saveBackstep

moveAction : Dir.Dir -> Game -> Game
moveAction dir g = chain (leave dir) g <| chain (Maybe.map afterAction << movePlayer dir) g <| failMove g

onRoom : (Room -> Room) -> Game -> Game
onRoom f model = { model | room = f model.room }

saveBackstep : Game -> Game
saveBackstep game = { game | backsteps = [game.room] }

afterAction : Game -> Game
afterAction = ifAlive updateRoom << ifAlive (aiTurn << triggerSword)

chain : (a -> Maybe a) -> a -> a -> a
chain f i a = Maybe.withDefault a (f i)

movePlayer : Dir.Dir -> Game -> Maybe Game
movePlayer dir = canDo (moveTo dir) (onRoom (Room.movePlayer dir))

-- TODO : Simplify
moveTo : Dir.Dir -> Game -> Bool
moveTo dir { room } =
  canRPlayerMoveTo room.playerCoord room (Dir.moveCoord room.width room.playerCoord dir)

failMove : Game -> Game
failMove = afterAction


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

leave : Dir.Dir -> Game -> Maybe Game
leave dir game =
  let
    dest = destPos dir game.room
    x = roomDelta dest game.room
    adj = add x game.level.currentRoomId
  in
  Maybe.withDefault Nothing <| canDo (Room.isOut dest << .room) (goToRoom x << updateLevels adj) game

updateLevels : Point -> Game -> Game
updateLevels roomId game =
  { game
  | level = Level.move (Room.isClear game.room) roomId game.level
  }

goToRoom : Point -> Game -> Maybe Game
goToRoom pos game = Maybe.map (setRoom game) (Level.buildCurrentRoom (Room.shiftPos pos game.room) game.room.playerDir game.level)

setRoom : Game -> Room -> Game
setRoom game room = { game | room = room, checkpoints = [room], backsteps = [] }

-- withMAction : (Room -> MoveResult) -> Model -> Model
-- withMAction action model = model
--   case action model.currentRoom of
--     Move room -> withAction (const room) model
--     Leave delta newPlayerPos ->
--       let newRoomId = add model.level.currentRoomId delta in
--       case Dict.get newRoomId model.level.rooms of
--         Just { builder, state } ->
--           let
--             nextLevel =
--               postProcessRoom <|
--               Level.buildRoom
--                 (state == Level.Complete)
--                 newPlayerPos
--                 model.currentRoom.playerDir
--                 builder
--           in
--           { model
--           | effect = Just <| ChangeRoom nextLevel 0 delta
--           , currentRoom = concatLevels model.currentRoom nextLevel delta
--           , level = Level.move (List.isEmpty model.currentRoom.creatures) newRoomId model.level
--           }
--         Nothing -> withAction (const model.currentRoom) model

-- withAction : (Room -> Room) -> Model -> Model
-- withAction action model = model
  -- if model.playerAlive then
  --   let
  --     actualModel =
  --       onRoom checkSword
  --       <| isAlivePlayer
  --       <| onRoom (buildSwordPos << action)
  --       { model
  --       | backsteps = [model.currentRoom]
  --       , checkpoints =
  --         if model.justLoaded
  --           then model.currentRoom :: model.checkpoints
  --           else model.checkpoints
  --       , justLoaded = False
  --       }

  --     afterActionModel =
  --       afterAIPostProcess
  --       <| List.foldl
  --         (\creature md -> isAlivePlayer <| creatureTurn creature md)
  --         actualModel
  --         <| List.sortBy
  --             (squareDistanceToPlayer actualModel.currentRoom)
  --             actualModel.currentRoom.creatures
  --   in
  --     if afterActionModel.currentRoom.playerCoord == model.currentRoom.playerCoord
  --       then afterActionModel
  --       else postProcessTile afterActionModel
  -- else
  --   model

swordToggle : Room -> Room
swordToggle room =
  case Array.get room.swordPos room.blueprint of
    Just (Orb actions) -> Room.mapRoomTiles (orbAction actions) room
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

triggerSword : Game -> Game
triggerSword = onRoom (swordToggle << swordKill)

swordKill : Room -> Room
swordKill room = { room | creatures = List.filter ((/=) room.swordPos) room.creatures }

updateRoom : Game -> Game
updateRoom = updateCheckPoint << ifClear (onRoom (Room.toogleGreenDoor))

updateCheckPoint : Game -> Game
updateCheckPoint game =
  case Array.get game.room.playerCoord game.room.blueprint of
    Just Checkpoint ->
      { game
      | checkpoints = game.room :: game.checkpoints
      }

    _ -> game

ifClear : (Game -> Game) -> Game -> Game
ifClear f game = if Room.isClear game.room then f game else game

aiTurn : Game -> Game
aiTurn game = List.foldl creatureTurn game game.room.creatures

ifAlive : (Game -> Game) -> Game -> Game
ifAlive f game = if game.alive then f game else game

creatureTurn : Creature -> Game -> Game
creatureTurn creature = ifAlive (updateAlive << onRoom (AI.roach creature))

updateAlive : Game -> Game
updateAlive game = { game | alive = List.all ((/=) game.room.playerCoord) game.room.creatures }
