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
import Utils exposing (..)

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias Model =
  { level : Room
  , playerAlive : Bool
  , animationTick : Int
  , backsteps : List Room
  , checkpoints : List Room
  , justLoaded : Bool
  , effect : Maybe Effect
  , levelsRepository : Dict.Dict (Int, Int) (Point -> Dir -> Room)
  }

type Effect = TileClicked Tile | ChangeRoom Room Point Int Point

type Image = Atlas | BaseMeph | Constructions | Lomem

type Msg = KeyPress String | Tick | Click Tile | AnimationRate Float

init : () -> ( Model, Cmd.Cmd Msg )
init () =
  let
    level = level2 (36, 27) E -- (6, 14) S -- (15, 0)
  in
  ( { level = level
    , playerAlive = True
    , animationTick = 0
    , backsteps = []
    , checkpoints = [level]
    , justLoaded = False
    , effect = Nothing
    , levelsRepository = Dict.fromList [((0, 0), level1), ((0, 1), level2), ((0, 2), level3), ((1, 1), level4)]
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

tickEffect : Float -> Model -> Model
tickEffect _ model =
  case model.effect of
    Just (ChangeRoom level offset s delta) ->
      if s >= 32
        then
          { model
          | effect = Nothing
          , level = level
          , backsteps = []
          , checkpoints = [level]
          , justLoaded = False
          }
        else
          { model
          | effect = Just <| ChangeRoom level (add offset delta) (s + 1) delta
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
  case action model.level of
    Move level -> withAction (const level) model
    Leave delta newPlayerPos ->
      case Dict.get (add model.level.pos delta) model.levelsRepository of
        Just level ->
          let nextLevel = level newPlayerPos model.level.playerDir
          in
          { model
          | effect = Just <| ChangeRoom
              nextLevel
              (Basics.max 0 (-38 * fst delta), Basics.max 0 (-32 * snd delta))
              0
              delta
          , level = concatLevels model.level nextLevel delta
          }
        Nothing -> withAction (const model.level) model

withAction : (Room -> Room) -> Model -> Model
withAction action model =
  if model.playerAlive then
    let
      actualModel =
        onLevel checkSword
        <| isAlivePlayer
        <| onLevel (buildSwordPos << action)
        { model
        | backsteps = [model.level]
        , checkpoints =
          if model.justLoaded
            then model.level :: model.checkpoints
            else model.checkpoints
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

onLevel : (Room -> Room) -> Model -> Model
onLevel f model =
  { model
  | level = f model.level
  }

isAlivePlayer : Model -> Model
isAlivePlayer model =
  { model
  | playerAlive = List.all ((/=) model.level.playerCoord) model.level.creatures
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch <| case model.effect of
    Just (ChangeRoom _ _ _ _) ->
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
    <| case model.effect of
      Just (ChangeRoom _ offset _ _) ->
        [ drawRoom offset model.level
        , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
        ]
      _ ->
        [ lazy (drawRoom (0, 0)) model.level
        , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
        ]

drawRoom : Point -> Room -> Html Msg
drawRoom offset level =
  svg
    [ width "1216"
    , height "1024"
    , viewBox "0 0 1216 1024"
    , Html.Attributes.style "display" "block"
    ]

    (
      (rect
        [ x "0"
        , y "0"
        , width "1216"
        , height "1024"
        , fill "rgb(75, 73, 75)"
        ] []) ::
      List.concat (
        Array.toList
        <| Array.map (tileTags offset level)
        <| Array.filter (visible offset)
        <| Array.indexedMap pair level.blueprint
        )
      -- ++ activeEffects model
    )

visible : Point -> (Coord, Tile) -> Bool
visible (ox, oy) (i, tile) =
  let
    (x, y) = toCoords 38 i
  in
    ox <= x && x < ox + 38 && oy <= y && y < oy + 32

tileTags : Point -> Room -> (Coord, Tile) -> List (Html Msg)
tileTags offset level (i, tag) =
  tileBackground offset level i tag ++ tileObjects offset i level

tileBackground : Point -> Room -> Coord -> Tile -> List (Html Msg)
tileBackground offset level i tile =
  let
    (px, py) = toCoords level.width i
    displayPos = Utils.sub (px, py) offset

    background =
      even ((modBy 2 px) + even py 0 1) (6, 5) (6, 4)

    pos = case tile of
      Floor -> background
      Wall -> Maybe.withDefault background (Array.get i level.wallTiles)
      Orb _ -> background
      Checkpoint -> background
      Obstical _ Pushed -> background
      Obstical _ InGround -> background
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
          [ x (String.fromInt (fst displayPos * 32))
          , y (String.fromInt (snd displayPos * 32))
          , width "32"
          , height "32"
          , viewBox "400 400 50 50"
          ]
          [ Svg.image [ xlinkHref "/assets/kenney/sheet_white1x.png" ] [] ]
        ]
      Obstical _ InGround ->
        [ imgTile displayPos (7, 12) Atlas ]
      Obstical _ Pushed ->
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
tileObjects offset i level =
  let p = Utils.sub (toCoords level.width i) offset
  in
  if List.member i level.creatures
    then
      --[ imgTile p i (cycle (0, 8) [(1, 8), (2, 8)] animationTick) Atlas ]
      [ imgTile p (0, 8) Atlas ]
    else if level.swordPos == i
      then [ imgTile p (8, 15) Atlas ]
      else
        if level.playerCoord == i
        then [ imgTile p (6, 4) Atlas ]
        else []

img50Tile : (Int, Int) -> (Int, Int) -> Html Msg
img50Tile (ix, iy) (px, py) =
  svg
    [ x (String.fromInt (ix * 32))
    , y (String.fromInt (iy * 32))
    , width "32"
    , height "32"
    , viewBox (String.fromInt (50 * px) ++ " " ++ String.fromInt (50 * py) ++ " 50 50")
    ]
    [ Svg.image [ xlinkHref "/assets/kenney/sheet_white1x.png" ] [] ]

imgTile : (Int, Int) -> (Int, Int) -> Image -> Html Msg
imgTile (ix, iy) (px, py) image =
  svg
    [ x (String.fromInt (ix * 32))
    , y (String.fromInt (iy * 32))
    , width "32"
    , height "32"
    , viewBox (String.fromInt (32 * px) ++ " " ++ String.fromInt (32 * py) ++ " 32 32")
    ]
    [ Svg.image
      [ xlinkHref <| case image of
          Atlas -> "/assets/underworld_load/underworld_load-atlas-32x32.png"
          BaseMeph -> "/assets/MephTileset/_Meph_32x32.png"
          Constructions -> "/assets/MephTileset/_Meph_constructions.png"
          Lomem -> "/assets/underworld_load/underworld_load-lomem-32x32.png"
      ] []
    ]

activeEffects : Model -> List (Html Msg)
activeEffects model =
  case model.effect of
    Just (TileClicked (Orb actions)) ->
      [
        rect [ x "0", y "0", width "32", height "32", fillOpacity "0.5", fill "yellow" ] []
      ]
    _ -> []
