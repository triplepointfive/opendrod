module Level exposing (..)

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
  }

type alias Level a =
  { currentRoomId : RoomId
  , complete : Bool
  , rooms : Dict.Dict RoomId (Room a)
  }

testLevel : Level (Point -> Dir -> Room.Room)
testLevel =
  { currentRoomId = (0, 1)
  , complete = False
  , rooms = Dict.fromList
    [ ((0, 0), { state = Seen, secret = False, builder = level1 })
    , ((0, 1), { state = Complete, secret = False, builder = level2 })
    , ((0, 2), { state = Seen, secret = False, builder = level3 })
    , ((1, 1), { state = Unseen, secret = False, builder = level4 })
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
