import Array
import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (href)
import Maybe
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias Coord = Int

type alias ObsticalId = Int

type ObsticalState = Pushed | InGround

type OrbAction = Close | Toggle | Open

type Tile = Wall | Floor | Orb (List (ObsticalId, OrbAction)) | Checkpoint | Obstical ObsticalId ObsticalState

type alias Creature = Coord

type alias Level =
  { blueprint : Array.Array Tile
  , creatures : List Creature
  , swordPos : Coord
  , playerCoord : Coord
  , playerDir : Dir
  }

type alias Model =
  { level : Level
  , playerAlive : Bool
  , animationTick : Int
  , backsteps : List Level
  , checkpoints : List Level
  , justLoaded : Bool
  }

type Dir = N | NE | E | SE | S | SW | W | NW

-- HTML

type Msg = KeyPress String | Tick | Undo

w = 5

init : () -> ( Model, Cmd.Cmd Msg )
init () =
  let
    level =
      { blueprint =
        Array.fromList
          <| List.concat
            [ [Wall, Wall, Wall, Wall, Wall]
            , [Wall, Floor, Floor, Orb [(0, Open)], Wall]
            , [Wall, Floor, Floor, Floor, Wall]
            , [Wall, Floor, Floor, Orb [(1, Toggle), (2, Toggle)], Wall]
            , [Wall, Floor, Floor, Floor, Wall]
            , [Wall, Floor, Floor, Orb [(2, Close)], Wall]
            , [Wall, Floor, Floor, Floor, Wall]
            , [Wall, Wall, Obstical 0 Pushed, Wall, Wall]
            , [Wall, Wall, Floor, Wall, Wall]
            , [Wall, Wall, Obstical 1 InGround, Wall, Wall]
            , [Wall, Wall, Floor, Wall, Wall]
            , [Wall, Wall, Obstical 2 Pushed, Wall, Wall]
            , [Wall, Wall, Floor, Wall, Wall]
            , [Wall, Wall, Wall, Wall, Wall]
            ]
      , creatures = [] -- [29, 30, 33, 34, 25, 26]
      , swordPos = 11
      , playerCoord = 6
      , playerDir = S
      }
  in
  ( { level = level
    , playerAlive = True
    , animationTick = 0
    , backsteps = []
    , checkpoints = [level]
    , justLoaded = False
    }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick -> ( tick model , Cmd.none )

    KeyPress "Backspace" -> ( undo model , Cmd.none )
    KeyPress "r" -> ( loadCheckpoint model , Cmd.none )

    KeyPress "q" -> ( withAction (turn dirLeft) model , Cmd.none )
    KeyPress "w" -> ( withAction (turn dirRight) model , Cmd.none )

    KeyPress "j" -> ( withAction (playerMoveDir S) model, Cmd.none )
    KeyPress "k" -> ( withAction (playerMoveDir N) model, Cmd.none )
    KeyPress "l" -> ( withAction (playerMoveDir E) model, Cmd.none )
    KeyPress "h" -> ( withAction (playerMoveDir W) model, Cmd.none )

    KeyPress "y" -> ( withAction (playerMoveDir NW) model, Cmd.none )
    KeyPress "u" -> ( withAction (playerMoveDir NE) model , Cmd.none )
    KeyPress "b" -> ( withAction (playerMoveDir SW) model, Cmd.none )
    KeyPress "n" -> ( withAction (playerMoveDir SE) model , Cmd.none )

    _ -> ( model , Cmd.none )

undo : Model -> Model
undo model =
  case model.backsteps of
    x :: xs ->
      { model
      | level = x
      , backsteps = xs
      , playerAlive = True -- TODO: Consider moving to level
      }
    _ -> model

loadCheckpoint : Model -> Model
loadCheckpoint model =
  case model.checkpoints of
    x :: xs ->
      { model
      | level = x
      , checkpoints = xs
      , justLoaded = True
      , playerAlive = True
      }

    _ -> model

tick : Model -> Model
tick model =
  { model
  | animationTick = model.animationTick + 1
  }

withAction : (Level -> Level) -> Model -> Model
withAction action model =
  if model.playerAlive then
    let
      actualModel =
        onLevel checkSword
        <| isAlivePlayer
        <| onLevel (buildSwordPos << action)
        { model
        | backsteps = [model.level]
        , checkpoints = if model.justLoaded then model.level :: model.checkpoints else model.checkpoints
        , justLoaded = False
        }

      afterActionModel =
        List.foldl
          (\creature md -> isAlivePlayer <| creatureTurn creature md)
          actualModel
          <| List.sortBy
              (squareDistanceToPlayer actualModel.level)
              actualModel.level.creatures
    in
      if afterActionModel.level.playerCoord == model.level.playerCoord
        then afterActionModel
        else postProcessTile afterActionModel
  else
    model

postProcessTile : Model -> Model
postProcessTile model =
  case Array.get model.level.playerCoord model.level.blueprint of
    Just Checkpoint ->
      { model
      | checkpoints = model.level :: model.checkpoints
      }

    _ -> model

creatureTurn : Creature -> Model -> Model
creatureTurn creature model =
  if model.playerAlive
    then onLevel (roachAI creature) model
    else model

onLevel : (Level -> Level) -> Model -> Model
onLevel f model =
  { model
  | level = f model.level
  }

roachAI : Creature -> Level -> Level
roachAI coord level =
  if List.member coord level.creatures
    then
      let dx = sign <| (modBy w level.playerCoord) - (modBy w coord)
          dy = w * sign ((level.playerCoord // w) - (coord // w))
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

buildSwordPos : Level -> Level
buildSwordPos level =
  { level
  | swordPos = dirCoord level.playerCoord level.playerDir
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

isAlivePlayer : Model -> Model
isAlivePlayer model =
  { model
  | playerAlive = List.all ((/=) model.level.playerCoord) model.level.creatures
  }

turn : (Dir -> Dir) -> Level -> Level
turn newDir level =
  { level | playerDir = newDir level.playerDir }

playerMoveDir : Dir -> Level -> Level
playerMoveDir dir level =
  let destPos = dirCoord level.playerCoord dir
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

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ onKeyDown keyDecoder
    , Time.every 500 (const Tick)
    ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map KeyPress
    <| Decode.field "key" Decode.string

view : Model -> Html Msg
view model =
  div []
    [ svg
        [ width "1024", height "1024", viewBox "0 0 1024 1024" ]
        (
          List.concat
            <| Array.toList (Array.indexedMap (tileTags model) model.level.blueprint)
        )
    , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
    ]

tileTags : Model -> Coord -> Tile -> List (Html Msg)
tileTags model i tag = tileBackground i tag ++ tileObjects i model

tileBackground : Coord -> Tile -> List (Html Msg)
tileBackground i tile =
  let
    (px, py) = toCoords i

    background =
      even
        ((modBy 2 px) + even py 0 1)
        "192 160 32 32"
        "192 128 32 32"

    backgroundBox = case tile of
      Floor -> background
      Wall -> "0 32 32 32"
      Orb _ -> background
      Checkpoint -> background
      Obstical _ Pushed -> background
      Obstical _ InGround -> background

    tileItems = case tile of
      Floor -> []
      Wall -> []
      Orb _ ->
        [ svg
          [ x (String.fromInt (px * 32))
          , y (String.fromInt (py * 32))
          , width "32"
          , height "32"
          , viewBox "320 320 32 32"
          ]
          [ Svg.image [ xlinkHref "/assets/underworld_load/underworld_load-atlas-32x32.png" ] [] ]
        ]
      Checkpoint ->
        [ svg
          [ x (String.fromInt (px * 32))
          , y (String.fromInt (py * 32))
          , width "32"
          , height "32"
          , viewBox "400 400 50 50"
          ]
          [ Svg.image [ xlinkHref "/assets/kenney/sheet_white1x.png" ] [] ]
        ]
      Obstical _ InGround ->
        [ svg
          [ x (String.fromInt (px * 32))
          , y (String.fromInt (py * 32))
          , width "32"
          , height "32"
          , viewBox "224 384 32 32"
          ]
          [ Svg.image [ xlinkHref "/assets/underworld_load/underworld_load-atlas-32x32.png" ] [] ]
        ]
      Obstical _ Pushed ->
        [ svg
          [ x (String.fromInt (px * 32))
          , y (String.fromInt (py * 32))
          , width "32"
          , height "32"
          , viewBox "256 384 32 32"
          ]
          [ Svg.image [ xlinkHref "/assets/underworld_load/underworld_load-atlas-32x32.png" ] [] ]
        ]
  in
    [ svg
      [ x (String.fromInt ((modBy w i) * 32))
      , y (String.fromInt ((i // w) * 32))
      , width "32"
      , height "32"
      , viewBox backgroundBox
      ]
      [ Svg.image [ xlinkHref "/assets/underworld_load/underworld_load-lomem-32x32.png" ] [] ]
    ] ++ tileItems

tileObjects : Coord -> Model -> List (Html Msg)
tileObjects i model =
  let
    atlas pos =
      svg
        [ x (String.fromInt ((modBy w i) * 32))
        , y (String.fromInt ((i // w) * 32))
        , width "32"
        , height "32"
        , viewBox pos
        ]
        [ Svg.image
          [ xlinkHref "/assets/underworld_load/underworld_load-atlas-32x32.png" ] []
        ]
  in
    if List.member i model.level.creatures
       then [ atlas
        <| ordered "0 256 32 32" ["32 256 32 32", "64 256 32 32"] model.animationTick ]
       else if model.level.swordPos == i
         then [ atlas "256 480 32 32" ]
         else
           if model.level.playerCoord == i
           then [ atlas "192 128 32 32" ]
           else []

dirCoord : Coord -> Dir -> Coord
dirCoord coord dir =
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

toCoords : Int -> (Int, Int)
toCoords i = (modBy w i , i // w)

squareDistanceToPlayer : Level -> Creature -> Int
squareDistanceToPlayer level coords =
  let
    (px, py) = toCoords level.playerCoord
    (x, y) = toCoords coords
    (dx, dy) = (px - x, py - y)
  in
    dx * dx + dy * dy

-- Utils

ordered : a -> List a -> Int -> a
ordered x xs i =
  case List.drop (modBy (1 + List.length xs) i) xs of
    a :: _ -> a
    _ -> x

sign : Int -> Int
sign x =
  if x > 0
    then 1
    else if x < 0
      then -1
      else 0

const : a -> b -> a
const x _ = x

even : Int -> a -> a -> a
even i t f =
  if modBy 2 i == 0 then t else f
