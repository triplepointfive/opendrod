import Array
import Browser
import Browser.Events exposing (onKeyDown, onAnimationFrameDelta)
import Debug
import Dict
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes
import Html.Lazy exposing (lazy)
import Maybe
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

import AI exposing (..)
import Room exposing (..)
import Rooms exposing (..)
import Level
import Utils exposing (..)

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

tileDim : Int
tileDim = 32

tileD : String
tileD = String.fromInt tileDim

type alias Model =
  { currentRoom : Room
  , playerAlive : Bool
  , animationTick : Int
  , backsteps : List Room
  , checkpoints : List Room
  , justLoaded : Bool
  , effect : Maybe Effect
  , levelsRepository : Dict.Dict (Int, Int) (Point -> Dir -> Room)
  , level : Level.Level (Point -> Dir -> Room)
  }

type Effect = TileClicked Tile | ChangeRoom Room Float Point

type Image = Atlas | BaseMeph | Constructions | Lomem

type Msg = KeyPress String | Tick | Click Tile | AnimationRate Float

init : () -> ( Model, Cmd.Cmd Msg )
init () =
  let
    currentRoom = level2 (36, 27) E -- (6, 14) S -- (15, 0)
  in
  ( { currentRoom = currentRoom
    , playerAlive = True
    , animationTick = 0
    , backsteps = []
    , checkpoints = [currentRoom]
    , justLoaded = False
    , effect = Nothing
    , levelsRepository = Dict.fromList [((0, 0), level1), ((0, 1), level2), ((0, 2), level3), ((1, 1), level4)]
    , level = Level.testLevel
    }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick -> ( tick model , Cmd.none )
    AnimationRate delta -> ( tickEffect delta model, Cmd.none )

    KeyPress "Backspace" -> ( undo model , Cmd.none )
    KeyPress "r" -> ( loadCheckpoint model , Cmd.none )

    KeyPress "q" -> ( withAction (turn dirLeft) model , Cmd.none )
    KeyPress "w" -> ( withAction (turn dirRight) model , Cmd.none )

    KeyPress "j" -> ( withMAction (playerMoveDir S) model, Cmd.none )
    KeyPress "k" -> ( withMAction (playerMoveDir N) model, Cmd.none )
    KeyPress "l" -> ( withMAction (playerMoveDir E) model, Cmd.none )
    KeyPress "h" -> ( withMAction (playerMoveDir W) model, Cmd.none )

    KeyPress "y" -> ( withMAction (playerMoveDir NW) model, Cmd.none )
    KeyPress "u" -> ( withMAction (playerMoveDir NE) model , Cmd.none )
    KeyPress "b" -> ( withMAction (playerMoveDir SW) model, Cmd.none )
    KeyPress "n" -> ( withMAction (playerMoveDir SE) model , Cmd.none )

    KeyPress _ -> ( model, Cmd.none )

    Click tile -> ( addClickTileEffect tile model, Cmd.none )

undo : Model -> Model
undo model =
  case model.backsteps of
    x :: xs ->
      { model
      | currentRoom = x
      , backsteps = xs
      , playerAlive = True -- TODO: Consider moving to currentRoom
      }
    _ -> model

loadCheckpoint : Model -> Model
loadCheckpoint model =
  case model.checkpoints of
    x :: xs ->
      { model
      | currentRoom = x
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

tickEffect : Float -> Model -> Model
tickEffect tickDelta model =
  case model.effect of
    Just (ChangeRoom room s delta) ->
      let resState = s + tickDelta / 900 in
      if resState >= 1
        then
          { model
          | effect = Nothing
          , currentRoom = room
          , backsteps = []
          , checkpoints = [room]
          , justLoaded = False
          }
        else
          { model
          | effect = Just <| ChangeRoom room resState delta
          }

    _ -> model

addClickTileEffect : Tile -> Model -> Model
addClickTileEffect tile model =
  case tile of
    Orb _ -> { model | effect = Just (TileClicked tile)}
    _ -> { model | effect = Nothing }

creatureTurn : Creature -> Model -> Model
creatureTurn creature model =
  if model.playerAlive
    then onLevel (roachAI creature) model
    else model

withMAction : (Room -> MoveResult) -> Model -> Model
withMAction action model =
  case action model.currentRoom of
    Move room -> withAction (const room) model
    Leave delta newPlayerPos ->
      case Dict.get (add model.currentRoom.pos delta) model.levelsRepository of
        Just room ->
          let nextLevel = room newPlayerPos model.currentRoom.playerDir
          in
          { model
          | effect = Just <| ChangeRoom
              nextLevel
              0
              delta
          , currentRoom = concatLevels model.currentRoom nextLevel delta
          }
        Nothing -> withAction (const model.currentRoom) model

withAction : (Room -> Room) -> Model -> Model
withAction action model =
  if model.playerAlive then
    let
      actualModel =
        onLevel checkSword
        <| isAlivePlayer
        <| onLevel (buildSwordPos << action)
        { model
        | backsteps = [model.currentRoom]
        , checkpoints =
          if model.justLoaded
            then model.currentRoom :: model.checkpoints
            else model.checkpoints
        , justLoaded = False
        }

      afterActionModel =
        List.foldl
          (\creature md -> isAlivePlayer <| creatureTurn creature md)
          actualModel
          <| List.sortBy
              (squareDistanceToPlayer actualModel.currentRoom)
              actualModel.currentRoom.creatures
    in
      if afterActionModel.currentRoom.playerCoord == model.currentRoom.playerCoord
        then afterActionModel
        else postProcessTile afterActionModel
  else
    model

postProcessTile : Model -> Model
postProcessTile model =
  case Array.get model.currentRoom.playerCoord model.currentRoom.blueprint of
    Just Checkpoint ->
      { model
      | checkpoints = model.currentRoom :: model.checkpoints
      }

    _ -> model

onLevel : (Room -> Room) -> Model -> Model
onLevel f model =
  { model
  | currentRoom = f model.currentRoom
  }

isAlivePlayer : Model -> Model
isAlivePlayer model =
  { model
  | playerAlive = List.all ((/=) model.currentRoom.playerCoord) model.currentRoom.creatures
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch <| case model.effect of
    Just (ChangeRoom _ _ _) ->
      [ onAnimationFrameDelta AnimationRate
      ]

    _ ->
      [ onKeyDown keyDecoder
      ]
    -- , Time.every 500 (const Tick)

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map KeyPress
    <| Decode.field "key" Decode.string

view : Model -> Html Msg
view model =
  div []
    [ case model.effect of
      Just (ChangeRoom _ c (dx, dy)) ->
        let f s max = max * s * sin( c * pi / 2 + pi / 4 * (s - 1)) in
        -- let f s max = max * s * (0.5 * (s - 1) + c) in
        drawRoom
          ( round <| f (toFloat dx) 1216
          , round <| f (toFloat dy) 1024
          )
          model.currentRoom
      _ ->
        drawRoom (0, 0) model.currentRoom
    , drawMinimap model.level
    , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
    ]

drawMinimap : Level.Level a -> Html Msg
drawMinimap level =
  svg
    [ width "100"
    , height "100"
    , viewBox "0 0 120 120"
    ]
    <| (rect [ x "0" , y "0" , width "120" , height "120" , fill "rgb(75, 73, 75)" ] [])
        :: (Dict.values <| Dict.map (drawMinimapRoom level.currentRoom) level.rooms)
        ++ [rect [ x "0", y "0", width "120", height "120", stroke "gold", strokeWidth "5", fill "none" ] []]

drawMinimapRoom : Room.RoomId -> Room.RoomId -> Level.Room a -> Html Msg
drawMinimapRoom (ox, oy) (dx, dy) room =
  rect
    [ x <| String.fromInt <| 45 + 30 * (dx - ox)
    , y <| String.fromInt <| 45 + 30 * (dy - oy)
    , width "30"
    , height "30"
    , fill <| case room.state of
      Level.Unseen -> "none"
      Level.Complete -> "white"
      Level.Seen -> "red"
    , stroke <| if ox == dx && oy == dy then "gold" else "none"
    , strokeWidth "3"
    ] []

drawRoom : Point -> Room -> Html Msg
drawRoom (dx, dy) room =
  svg
    [ width "1216"
    , height "1024"
    , viewBox <| String.fromInt dx ++ " " ++ String.fromInt dy ++ " 1216 1024"
    ]
    (
      (rect
        [ x "0"
        , y "0"
        , width <| String.fromInt (1216 * 2)
        , height <| String.fromInt (1024 * 2)
        , fill "rgb(75, 73, 75)"
        ] []) ::
      List.concat (
        Array.toList
        <| Array.map (tileTags (0, 0) room)
        <| Array.indexedMap pair room.blueprint
        )
      -- ++ activeEffects model
    )

tileTags : Point -> Room -> (Coord, Tile) -> List (Html Msg)
tileTags offset room (i, tag) =
  tileBackground offset room i tag ++ tileObjects offset i room

tileBackground : Point -> Room -> Coord -> Tile -> List (Html Msg)
tileBackground offset room i tile =
  let
    (px, py) = toCoords room.width i
    displayPos = Utils.sub (px, py) offset

    background =
      even ((modBy 2 px) + even py 0 1) (6, 5) (6, 4)

    pos = case tile of
      Floor -> background
      Wall -> Maybe.withDefault background (Array.get i room.wallTiles)
      Orb _ -> background
      Checkpoint -> background
      Door _ Closed -> background
      Door _ Open -> background
      Arrow _ -> background

    tileSet = case tile of
      Wall -> Constructions
      _ -> Lomem

    tileItems = case tile of
      Floor -> []
      Wall -> []
      Orb _ ->
        [ imgTile displayPos (4, 0) BaseMeph ]
      Checkpoint ->
        [ svg
          [ x (String.fromInt (fst displayPos * tileDim))
          , y (String.fromInt (snd displayPos * tileDim))
          , width tileD
          , height tileD
          , viewBox "400 400 50 50"
          ]
          [ Svg.image [ xlinkHref "assets/kenney/sheet_white1x.png" ] [] ]
        ]
      Door _ Open ->
        [ imgTile displayPos (7, 12) Atlas ]
      Door _ Closed ->
        [ imgTile displayPos (8, 12) Atlas ]

      Arrow N -> [img50Tile displayPos (0, 8)]
      Arrow S -> [img50Tile displayPos (1, 1)]
      Arrow W -> [img50Tile displayPos (1, 0)]
      Arrow E -> [img50Tile displayPos (0, 9)]
      Arrow NW -> [img50Tile displayPos (2, 0)]
      Arrow NE -> [img50Tile displayPos (1, 9)]
      Arrow SW -> [img50Tile displayPos (8, 5)]
      Arrow SE -> [img50Tile displayPos (8, 4)]
  in
    (if pos == (-1, -1) then [] else [ imgTile displayPos pos tileSet ]) ++ tileItems

tileObjects : Point -> Coord -> Room -> List (Html Msg)
tileObjects offset i room =
  let p = Utils.sub (toCoords room.width i) offset
  in
  if List.member i room.creatures
    then
      --[ imgTile p i (cycle (0, 8) [(1, 8), (2, 8)] animationTick) Atlas ]
      [ imgTile p (0, 8) Atlas ]
    else if room.swordPos == i
      then [ imgTile p (8, 15) Atlas ]
      else
        if room.playerCoord == i
        then [ imgTile p (6, 4) Atlas ]
        else []

img50Tile : (Int, Int) -> (Int, Int) -> Html Msg
img50Tile (ix, iy) (px, py) =
  svg
    [ x (String.fromInt (ix * tileDim))
    , y (String.fromInt (iy * tileDim))
    , width tileD
    , height tileD
    , viewBox (String.fromInt (50 * px) ++ " " ++ String.fromInt (50 * py) ++ " 50 50")
    ]
    [ Svg.image [ xlinkHref "assets/kenney/sheet_white1x.png" ] [] ]

imgTile : (Int, Int) -> (Int, Int) -> Image -> Html Msg
imgTile (ix, iy) (px, py) image =
  svg
    [ x (String.fromInt (ix * tileDim))
    , y (String.fromInt (iy * tileDim))
    , width tileD
    , height tileD
    , viewBox (String.fromInt (tileDim * px) ++ " " ++ String.fromInt (tileDim * py) ++ " 32 32")
    ]
    [ Svg.image
      [ xlinkHref <| case image of
          Atlas -> "assets/underworld_load/underworld_load-atlas-32x32.png"
          BaseMeph -> "assets/MephTileset/Meph_32x32.png"
          Constructions -> "assets/MephTileset/Meph_constructions.png"
          Lomem -> "assets/underworld_load/underworld_load-lomem-32x32.png"
      ] []
    ]

activeEffects : Model -> List (Html Msg)
activeEffects model =
  case model.effect of
    Just (TileClicked (Orb actions)) ->
      [
        rect
          [ x "0"
          , y "0"
          , width tileD
          , height tileD
          , fillOpacity "0.5"
          , fill "yellow"
          ]
          []
      ]
    _ -> []
