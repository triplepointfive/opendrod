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

import Level exposing (..)
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
  , wallTiles : Array.Array (Int, Int)
  , effect : Maybe Effect
  }

type Effect = TileClicked Tile

type Image = Atlas | Constructions | Lomem

type Msg = KeyPress String | Tick | Click Tile

init : () -> ( Model, Cmd.Cmd Msg )
init () =
  let
    walls = List.repeat 27 Wall
    level =
      { blueprint =
          buildBlueprint
            [ "################################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############       #############"
            , "############       #############"
            , "############       #############"
            , "############       #############"
            , "############       #############"
            , "############       #############"
            , "############       #############"
            , "############       #############"
            , "############       #############"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            , "############### ################"
            ]
            [ (337, Orb [(0, Open)])
            , (401, Orb [(1, Toggle), (2, Toggle)])
            , (465, Orb [(2, Close)])
            , (559, Obstical 0 Pushed)
            , (623, Obstical 1 InGround)
            , (687, Obstical 2 Pushed)
            ]
      , creatures = []
      , swordPos = 79
      , playerCoord = 47
      , playerDir = S
      , width = 32
      }
  in
  ( { level = level
    , playerAlive = True
    , animationTick = 0
    , backsteps = []
    , checkpoints = [level]
    , justLoaded = False
    , effect = Nothing
    , wallTiles = buildWalls level
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
    [ svg
        [ width "1024", height "1024", viewBox "0 0 1024 1024" ]
        (
          (rect [ x "0", y "0", width "1024", height "1024", fill "grey" ] []) ::
          List.concat (Array.toList (Array.indexedMap (tileTags model) model.level.blueprint))
          ++ activeEffects model
        )
    , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
    ]

tileTags : Model -> Coord -> Tile -> List (Html Msg)
tileTags model i tag =
  tileBackground model i tag ++
    tileObjects i model

tileBackground : Model -> Coord -> Tile -> List (Html Msg)
tileBackground model i tile =
  let
    (px, py) = toCoords model.level.width i

    background =
      even ((modBy 2 px) + even py 0 1) (6, 5) (6, 4)

    pos = case tile of
      Floor -> background
      Wall -> Maybe.withDefault background (Array.get i model.wallTiles)
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
        [ imgTile (px, py) i (10, 10) Atlas ]
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

tileObjects : Coord -> Model -> List (Html Msg)
tileObjects i model =
  let p = toCoords model.level.width i
  in
  if List.member i model.level.creatures
    then
    [ imgTile p i (cycle (0, 8) [(1, 8), (2, 8)] model.animationTick) Atlas ]
    else if model.level.swordPos == i
      then [ imgTile p i (8, 15) Atlas ]
      else
        if model.level.playerCoord == i
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

buildWalls : Level -> Array.Array (Int, Int)
buildWalls level =
  let
    layer = 0

    tileToWall coord tile =
      let w dir = Maybe.withDefault Wall (Array.get (dirCoord level.width coord dir) level.blueprint) == Wall
      in
      case tile of
        Wall ->
          case [w N, w NE, w E, w SE, w S, w SW, w W, w NW] of
            [True, True, True, True, True, True, True, True] -> (-1, -1)

            [True, True, True, _, False, _, True, True] -> (0, layer)
            [False, _, True, True, True, True, True, _] -> (0, layer)

            [True, _, False, _, True, True, True, True] -> (1, layer)
            [True, True, True, True, True, _, False, _] -> (1, layer)

            [True, False, True, True, True, True, True, True] -> (2, layer)
            [True, True, True, False, True, True, True, True] -> (3, layer)
            [True, True, True, True, True, False, True, True] -> (4, layer)
            [True, True, True, True, True, True, True, False] -> (5, layer)

            [True, True, True, True, False, False, False, False] -> (2, layer)
            [False, True, True, True, True, False, False, False] -> (3, layer)
            [False, False, False, False, True, True, True, True] -> (4, layer)
            [True, False, False, False, False, True, True, True] -> (5, layer)

            [True, True, True, False, False, False, False, False] -> (2, layer)
            [False, False, True, True, True, False, False, False] -> (3, layer)
            [False, False, False, False, True, True, True, False] -> (4, layer)
            [True, False, False, False, False, False, True, True] -> (5, layer)

            _ -> (3, 4)
        _ -> (0, 0)
  in
    Array.indexedMap tileToWall level.blueprint
