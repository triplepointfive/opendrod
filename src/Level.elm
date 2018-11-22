module Level exposing (..)

import Array
import Dict
import Maybe

import Room exposing (Dir, RoomId)
import Rooms exposing (..)
import Utils exposing (Point)

type RoomState = Complete | Seen | Unseen

type alias Room a =
  { state : RoomState
  , secret : Bool
  , builder : a
  , minimap : List Bool
  }

type alias Level a =
  { currentRoomId : RoomId
  , complete : Bool
  , rooms : Dict.Dict RoomId (Room a)
  }

buildMinimap : Room.Room -> List Bool
buildMinimap room =
  List.map (\tile -> tile == Room.Wall) <| Array.toList room.blueprint

testLevel : Level (Point -> Dir -> Room.Room)
testLevel =
  { currentRoomId = (0, 1)
  , complete = False
  , rooms = Dict.fromList
    [ ( (0, 0)
      , { state = Seen
        , secret = False
        , builder = level1
        , minimap = buildMinimap <| level1 (0, 0) Room.N
        }
      )
    , ( (0, 1)
      , { state = Complete
        , secret = False
        , builder = level2
        , minimap = buildMinimap <| level2 (0, 0) Room.N
        }
      )
    , ( (0, 2)
      , { state = Seen
        , secret = False
        , builder = level3
        , minimap = buildMinimap <| level3 (0, 0) Room.N
        }
      )
    , ( (1, 1)
      , { state = Unseen
        , secret = False
        , builder = level4
        , minimap = buildMinimap <| level4 (0, 0) Room.N
        }
      )
    ]
  }

-- TODO: set room complete state if it does not include enemies
enter : RoomId -> Level a -> Level a
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

leave : Bool -> Level a -> Level a
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

move : Bool -> RoomId -> Level a -> Level a
move cleared to level =
  leave cleared level |> enter to
