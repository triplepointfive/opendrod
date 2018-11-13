module Level exposing (..)

import Array

import Utils exposing (..)

type alias Coord = Int

type alias ObsticalId = Int

type ObsticalState
  = Pushed
  | InGround

type OrbAction
  = Close
  | Toggle
  | Open

type Tile
  = Wall
  | Floor
  | Orb (List (ObsticalId, OrbAction))
  | Checkpoint
  | Obstical ObsticalId ObsticalState

type alias Creature = Coord

type alias Level =
  { blueprint : Array.Array Tile
  , creatures : List Creature
  , swordPos : Coord
  , playerCoord : Coord
  , playerDir : Dir
  , width : Int
  , height : Int
  , pos : Point
  , wallTiles : Array.Array (Int, Int)
  }

type Dir = N | NE | E | SE | S | SW | W | NW

dirCoord : Int -> Coord -> Dir -> Coord
dirCoord w coord dir =
  coord + case dir of
    N  -> -w
    NE -> 1 - w
    E  -> 1
    SE -> w + 1
    S  -> w
    SW -> w - 1
    W  -> -1
    NW -> -w - 1

dirPoint : Point -> Dir -> Point
dirPoint (x, y) dir =
  let (dx, dy) = dirDelta dir
  in (x + dx, y + dy)

dirDelta : Dir -> Point
dirDelta dir =
  case dir of
    N  -> (0, -1)
    NE -> (1, -1)
    E  -> (1, 0)
    SE -> (1, 1)
    S  -> (0, 1)
    SW -> (-1, 1)
    W  -> (-1, 0)
    NW -> (-1, -1)

dirLeft : Dir -> Dir
dirLeft dir =
  case dir of
    N  -> NW
    NE -> N
    E  -> NE
    SE -> E
    S  -> SE
    SW -> S
    W  -> SW
    NW -> W

dirRight : Dir -> Dir
dirRight dir =
  case dir of
    N  -> NE
    NE -> E
    E  -> SE
    SE -> S
    S  -> SW
    SW -> W
    W  -> NW
    NW -> N

turn : (Dir -> Dir) -> Level -> Level
turn newDir level =
  { level | playerDir = newDir level.playerDir }

type MoveResult = Move Level | Leave Point Point

playerMoveDir : Dir -> Level -> MoveResult
playerMoveDir dir level =
  let
    destPos = dirCoord level.width level.playerCoord dir
    (px, py) = toCoords level.width level.playerCoord
    (x, y) = dirPoint (px, py) dir
    (lx, ly) = dirDelta dir
  in
    if canPlayerMoveTo destPos level
    then Move { level | playerCoord = destPos }
    else if x >= 0 && y >= 0 && x < level.width && y < level.height
      then Move level
      else
        let delta = ( if x < 0 || x >= level.width then (lx, 0) else (0, ly) ) in
        Leave
          delta

          ( modBy level.width (px + fst delta)
          , modBy level.height (py + snd delta)
          )

canMoveTo : Coord -> Level -> Bool
canMoveTo coord level =
  canPlayerMoveTo coord level && level.swordPos /= coord

canPlayerMoveTo : Coord -> Level -> Bool
canPlayerMoveTo coord level =
  let
    isUntaken = List.isEmpty <| List.filter ((==) coord) level.creatures
  in
  case Array.get coord level.blueprint of
    Nothing -> False
    Just Floor -> isUntaken
    Just Wall -> False
    Just (Orb _) -> False
    Just Checkpoint -> isUntaken
    Just (Obstical _ Pushed) -> False
    Just (Obstical _ InGround) -> isUntaken

buildSwordPos : Level -> Level
buildSwordPos level =
  { level
  | swordPos = dirCoord level.width level.playerCoord level.playerDir
  }

checkSword : Level -> Level
checkSword = swordToggle << swordKill

swordKill : Level -> Level
swordKill level =
  { level
  | creatures = List.filter ((/=) level.swordPos) level.creatures
  }

swordToggle : Level -> Level
swordToggle level =
  case Array.get level.swordPos level.blueprint of
    Just (Orb actions) ->
      { level
      | blueprint = Array.map (orbAction actions) level.blueprint
      }
    _ -> level

orbAction : List (ObsticalId, OrbAction) -> Tile -> Tile
orbAction actions tile =
  case tile of
    Obstical id state ->
      case List.filter (\(i, _) -> i == id) actions of
        (_, action) :: _ ->
          case action of
            Close -> Obstical id Pushed
            Open -> Obstical id InGround
            Toggle -> Obstical id (if state == Pushed then InGround else Pushed)
        _ -> tile
    _ -> tile

concatLevels : Level -> Level -> Point -> Level
concatLevels origin addend (dx, dy) =
  if dy < 0 then
    { origin
    | blueprint = Array.append addend.blueprint origin.blueprint
    , height    = origin.height + origin.height
    , wallTiles = Array.append addend.wallTiles origin.wallTiles
    , creatures = origin.creatures ++ addend.creatures
    }
  else
    { origin
    | blueprint = Array.append origin.blueprint addend.blueprint
    , height    = origin.height + origin.height
    , wallTiles = Array.append origin.wallTiles addend.wallTiles
    , creatures = origin.creatures ++ addend.creatures
    }
