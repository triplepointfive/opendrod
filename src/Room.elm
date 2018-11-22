module Room exposing (..)

import Array

import Utils exposing (..)

type alias Coord = Int

type alias ObsticalId = Int

type alias RoomId = Point

type ObsticalState
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
  | Door ObsticalId ObsticalState
  | GreenDoor ObsticalState
  | Arrow Dir

type alias Creature = Coord

type alias Room =
  { blueprint : Array.Array Tile
  , creatures : List Creature
  , swordPos : Coord
  , playerCoord : Coord
  , playerDir : Dir
  , width : Int
  , height : Int
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

deltaDir : Point -> Dir
deltaDir (dx, dy) =
  if dx > 0 && dy > 0 then SE
  else if dx > 0 && dy < 0 then NE
  else if dx > 0 then E
  else if dx < 0 && dy > 0 then SW
  else if dx < 0 && dy < 0 then NW
  else if dx < 0 then W
  else if dy > 0 then S
  else N -- dy < 0

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

oppositeDir : Dir -> Dir -> Bool
oppositeDir d1 d2 =
  case d1 of
    N -> List.member d2 [SW, S, SE]
    S -> List.member d2 [NW, N, NE]
    W -> List.member d2 [NE, E, SE]
    E -> List.member d2 [NW, W, SW]
    NE -> List.member d2 [W, S, SW]
    NW -> List.member d2 [E, S, SE]
    SE -> List.member d2 [W, N, NW]
    SW -> List.member d2 [E, N, NE]

turn : (Dir -> Dir) -> Room -> Room
turn newDir level =
  { level | playerDir = newDir level.playerDir }

type MoveResult = Move Room | Leave Point Point

playerMoveDir : Dir -> Room -> MoveResult
playerMoveDir dir level =
  let
    destPos = dirCoord level.width level.playerCoord dir
    (px, py) = toCoords level.width level.playerCoord
    (x, y) = dirPoint (px, py) dir
    (lx, ly) = dirDelta dir
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

canPlayerMoveTo : Coord -> Room -> Coord -> Bool
canPlayerMoveTo prevCoord level coord =
  let
    isUntaken = List.isEmpty <| List.filter ((==) coord) level.creatures
    dir = deltaDir (sub (toCoords level.width coord) (toCoords level.width prevCoord))

    canLeave = case Array.get prevCoord level.blueprint of
      Just (Arrow d) -> not (oppositeDir d dir)
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
    Just (Arrow d) -> not (oppositeDir d dir)

buildSwordPos : Room -> Room
buildSwordPos level =
  { level
  | swordPos = dirCoord level.width level.playerCoord level.playerDir
  }

checkSword : Room -> Room
checkSword = swordToggle << swordKill

swordKill : Room -> Room
swordKill level =
  { level
  | creatures = List.filter ((/=) level.swordPos) level.creatures
  }

swordToggle : Room -> Room
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
    Door id state ->
      case List.filter (\(i, _) -> i == id) actions of
        (_, action) :: _ ->
          Door id <| case action of
            Close -> Closed
            ToOpen -> Open
            Toggle -> if state == Closed then Open else Closed
        _ -> tile
    _ -> tile

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