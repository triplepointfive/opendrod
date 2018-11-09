import Array
import Browser
import Browser.Events exposing (onKeyDown)
import Debug
import Dict
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes
import Maybe
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time

import AI exposing (..)
import Level exposing (..)
import Levels exposing (..)
import Utils exposing (..)

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias Model =
  { level : Level
  , playerAlive : Bool
  , animationTick : Int
  , backsteps : List Level
  , checkpoints : List Level
  , justLoaded : Bool
  , effect : Maybe Effect
  , levelsRepository : Dict.Dict (Int, Int) (Point -> Dir -> Level)
  }

type Effect = TileClicked Tile | ChangeRoom Level Point Int Dir

type Image = Atlas | BaseMeph | Constructions | Lomem

type Msg = KeyPress String | Tick | Click Tile

init : () -> ( Model, Cmd.Cmd Msg )
init () =
  let
    level = level1 (15, 30) S
  in
  ( { level = level
    , playerAlive = True
    , animationTick = 0
    , backsteps = []
    , checkpoints = [level]
    , justLoaded = False
    , effect = Just (ChangeRoom level (0, 0) 0 S) -- Nothing
    , levelsRepository = Dict.fromList [((0, 0), level1), ((0, 1), level2)]
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

enterLevel : Level -> Model -> Model
enterLevel level model =
  { model
  | level = level
  , backsteps = []
  , checkpoints = [level]
  , justLoaded = False
  }

withMAction : (Level -> MoveResult) -> Model -> Model
withMAction action model =
  case action model.level of
    Move level -> withAction (const level) model
    Leave pos newPlayerPos ->
      case Dict.get pos model.levelsRepository of
        Just level -> enterLevel (level newPlayerPos model.level.playerDir) model
        Nothing -> withAction (const model.level) model

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

onLevel : (Level -> Level) -> Model -> Model
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
    <| case model.effect of
      Just (ChangeRoom _ (dx, dy) _ _) ->
        [ drawRoom (1024, 1024 - dy) model.level
        , drawRoom (1024, dy) model.level
        , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
        ]
      _ ->
        [ drawRoom (1024, 1024) model.level
        , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
        ]

drawRoom : Point -> Level -> Html Msg
drawRoom (w, h) level =
  svg
    [ width <| String.fromInt w
    , height <| String.fromInt h
    , viewBox
      <| String.concat
      <| List.intersperse " "
      <| List.map String.fromInt [1024 - w, 1024 - h, w, h]
    , Html.Attributes.style "display" "block"
    ]

    (
      (rect [ x "0", y "0", width "1024", height "1024", fill "grey" ] []) ::
      List.concat (Array.toList (Array.indexedMap (tileTags level) level.blueprint))
      -- ++ activeEffects model
    )

tileTags : Level -> Coord -> Tile -> List (Html Msg)
tileTags level i tag =
  tileBackground level i tag ++
    tileObjects i level

tileBackground : Level -> Coord -> Tile -> List (Html Msg)
tileBackground level i tile =
  let
    (px, py) = toCoords level.width i

    background =
      even ((modBy 2 px) + even py 0 1) (6, 5) (6, 4)

    pos = case tile of
      Floor -> background
      Wall -> Maybe.withDefault background (Array.get i level.wallTiles)
      Orb _ -> background
      Checkpoint -> background
      Obstical _ Pushed -> background
      Obstical _ InGround -> background

    tileSet = case tile of
      Wall -> Constructions
      _ -> Lomem

    tileItems = case tile of
      Floor -> []
      Wall -> []
      Orb _ ->
        [ imgTile (px, py) i (4, 0) BaseMeph ]
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
        [ imgTile (px, py) i (7, 12) Atlas ]
      Obstical _ Pushed ->
        [ imgTile (px, py) i (8, 12) Atlas ]
  in
    (if pos == (-1, -1) then [] else [ imgTile (px, py) i pos tileSet ]) ++ tileItems

tileObjects : Coord -> Level -> List (Html Msg)
tileObjects i level =
  let p = toCoords level.width i
  in
  if List.member i level.creatures
    then
      --[ imgTile p i (cycle (0, 8) [(1, 8), (2, 8)] animationTick) Atlas ]
      [ imgTile p i (0, 8) Atlas ]
    else if level.swordPos == i
      then [ imgTile p i (8, 15) Atlas ]
      else
        if level.playerCoord == i
        then [ imgTile p i (6, 4) Atlas ]
        else []

imgTile : (Int, Int) -> Coord -> (Int, Int) -> Image -> Html Msg
imgTile (ix, iy) i (px, py) image =
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
