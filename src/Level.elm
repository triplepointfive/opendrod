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
  { currentRoom : RoomId
  , complete : Bool
  , rooms : Dict.Dict RoomId (Room a)
  }

testLevel : Level (Point -> Dir -> Room.Room)
testLevel =
  { currentRoom = (0, 0)
  , complete = False
  , rooms = Dict.fromList
    [ ((0, 0), { state = Unseen, secret = False, builder = level1 })
    , ((0, 1), { state = Unseen, secret = False, builder = level2 })
    , ((0, 2), { state = Unseen, secret = False, builder = level3 })
    , ((1, 1), { state = Unseen, secret = False, builder = level4 })
    ]
  }

enter : Level a -> RoomId -> Level a
enter level id =
  { level
  | rooms = Dict.update id (Maybe.andThen (Just << enterState)) level.rooms
  }

enterState : Room a -> Room a
enterState room =
  { room
  | state = if room.state == Unseen then Seen else room.state
  }
