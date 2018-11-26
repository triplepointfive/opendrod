module Level exposing (..)

import Array
import Dict
import Maybe

import Dir
import Room exposing (..)
import Utils exposing (..)

type BuildTile = Tile Tile | Creature

type alias RoomBuilder =
  { blueprint : List String
  , repository : List (Char, BuildTile)
  }

type RoomState = Complete | Seen | Unseen

type alias RoomRep =
  { state : RoomState
  , secret : Bool
  , builder : RoomBuilder
  , minimap : List Bool
  }

type alias Level =
  { init : Room
  , currentRoomId : RoomId
  , complete : Bool
  , rooms : Dict.Dict RoomId RoomRep
  }

buildMinimap : RoomBuilder -> List Bool
buildMinimap =
  List.concatMap (List.map ((==) '#') << String.toList) << .blueprint

-- TODO: set room complete state if it does not include enemies
enter : RoomId -> Level -> Level
enter id level =
  let
    enterState room =
      { room
      | state = if room.state == Unseen then Seen else room.state
      }
  in
  { level
  | rooms = Dict.update id (Maybe.andThen (Just << enterState)) level.rooms
  , currentRoomId = id
  }

completeRoom : RoomId -> Level -> Level
completeRoom id level =
  let
    enterState room =
      { room
      | state = Complete
      }
  in
  { level
  | rooms = Dict.update id (Maybe.andThen (Just << enterState)) level.rooms
  , currentRoomId = id
  }

leave : Bool -> Level -> Level
leave cleared level =
  let
    leaveState room =
      { room
      | state = if cleared then Complete else room.state
      }
  in
  { level
  | rooms = Dict.update level.currentRoomId (Maybe.andThen (Just << leaveState)) level.rooms
  }

move : Bool -> RoomId -> Level -> Level
move cleared to level =
  leave cleared level |> enter to

buildCurrentRoom : Point -> Dir.Dir -> Level -> Maybe Room.Room
buildCurrentRoom pos dir level =
  case Dict.get level.currentRoomId level.rooms of
    Just { builder, state } -> Just <| buildRoom (state == Complete) pos dir builder
    Nothing -> Nothing

buildRoom : Bool -> Point -> Dir.Dir -> RoomBuilder -> Room.Room
buildRoom completed pos dir builder =
  let
    (blueprint, creatures) = buildBlueprint completed builder
  in
    Room.buildSwordPos { blueprint = blueprint
    , creatures = creatures
    , swordPos = toCoord pos 38
    , playerCoord = toCoord pos 38
    , playerDir = dir
    , width = 38
    , height = 32
    , wallTiles = buildWalls blueprint 38
    }

buildBlueprint : Bool -> RoomBuilder -> (Array.Array Tile, List Creature)
buildBlueprint completed { blueprint, repository } =
  let
    tilesMap = Dict.fromList repository

    buildTile (i, char) (tiles, creatures) =
      case Dict.get char tilesMap of
        Just (Tile t) -> (t :: tiles, creatures)
        Just Creature ->
          if completed
            then (Floor :: tiles, creatures)
            else (Floor :: tiles, i :: creatures)
        Nothing -> case char of
          '#' -> (Wall :: tiles, creatures)
          'X' -> (Checkpoint :: tiles, creatures)
          _ -> (Floor :: tiles, creatures)

    (builtTiles, builtCreatures) =
      List.foldr buildTile ([], [])
        <| List.indexedMap pair
        <| List.concatMap String.toList blueprint
  in
    ( Array.fromList builtTiles
    , builtCreatures
    )

buildWalls : Array.Array Tile -> Int -> Array.Array (Int, Int)
buildWalls blueprint width =
  let
    layer = 0

    tileToWall coord tile =
      let
        w dir =
          if modBy width coord == 0 && List.member dir [Dir.W, Dir.NW, Dir.SW] then True
          else if modBy width coord == width - 1 && List.member dir [Dir.E, Dir.NE, Dir.SE] then True
          else Maybe.withDefault Wall (Array.get (Dir.moveCoord width coord dir) blueprint) == Wall
      in
      case tile of
        Wall ->
          let walls = [w Dir.N, w Dir.NE, w Dir.E, w Dir.SE, w Dir.S, w Dir.SW, w Dir.W, w Dir.NW] in
          case walls of
            -- Fully surrounded
            [True, True, True, True, True, True, True, True] -> (-1, -1)

            -- Corners
            [True, False, True, True, True, True, True, True] -> (2, layer)
            [True, True, True, False, True, True, True, True] -> (3, layer)
            [True, True, True, True, True, False, True, True] -> (4, layer)
            [True, True, True, True, True, True, True, False] -> (5, layer)

            -- Corners
            [True, True, True, True, False, False, False, False] -> (2, layer)
            [False, True, True, True, True, False, False, False] -> (3, layer)
            [False, False, False, False, True, True, True, True] -> (4, layer)
            [True, False, False, False, False, True, True, True] -> (5, layer)

            -- Corners
            [True, True, True, False, False, False, False, False] -> (2, layer)
            [False, False, True, True, True, False, False, False] -> (3, layer)
            [False, False, False, False, True, True, True, False] -> (4, layer)
            [True, False, False, False, False, False, True, True] -> (5, layer)

            -- Horizontal
            [True, True, True, _, False, _, True, True] -> (0, layer)
            [False, _, True, True, True, True, True, _] -> (0, layer)
            [False, _, True, _, False, _, True, _] -> (0, layer)

            -- Vertical
            [True, _, False, _, True, True, True, True] -> (1, layer)
            [True, True, True, True, True, _, False, _] -> (1, layer)
            [True, _, False, _, True, _, False, _] -> (1, layer)

            -- T blocks
            [True, False, True, False, True, True, True, True] -> (6, layer)
            [True, True, True, True, True, False, True, False] -> (7, layer)
            [False, _, True, _, True, _, True, _] -> (8, layer)
            [True, _, True, _, False, _, True, _] -> (9, layer)

            -- Corners
            [True, _, True, _, False, _, False, _] -> (2, layer)
            [False, _, True, _, True, _, False, _] -> (3, layer)
            [False, _, False, _, True, _, True, _] -> (4, layer)
            [True, _, False, _, False, _, True, _] -> (5, layer)

            _ -> if (List.length <| List.filter w [Dir.N, Dir.E, Dir.S, Dir.W]) == 1
              then (12, layer) else (3, 4)
        _ -> (0, 0)
  in
    Array.indexedMap tileToWall blueprint
