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

playerMoveDir : Dir -> Level -> Level
playerMoveDir dir level =
  let destPos = dirCoord level.width level.playerCoord dir
  in
    if canPlayerMoveTo destPos level
    then { level | playerCoord = destPos }
    else level

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

roachAI : Creature -> Level -> Level
roachAI coord level =
  if List.member coord level.creatures
    then
      let dx = sign <| (modBy level.width level.playerCoord) - (modBy level.width coord)
          dy = level.width * sign ((level.playerCoord // level.width) - (coord // level.width))
          directMoves =
            List.map ((+) coord)
            <| [dx + dy] ++ (List.reverse <| List.sortBy (squareDistanceToPlayer level) [dy, dx])
          newCoord =
            case List.filter (\ i -> canMoveTo i level) directMoves of
              freeCoord :: _ -> freeCoord
              _ -> coord
      in
      { level
      | creatures = newCoord :: List.filter ((/=) coord) level.creatures
      }
    else
      level

squareDistanceToPlayer : Level -> Creature -> Int
squareDistanceToPlayer level coords =
  let
    (px, py) = toCoords level.width level.playerCoord
    (x, y) = toCoords level.width coords
    (dx, dy) = (px - x, py - y)
  in
    dx * dx + dy * dy

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
