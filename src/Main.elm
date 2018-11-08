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
  }

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
      , creatures = [42] -- [29, 30, 33, 34, 25, 26]
      , swordPos = 11
      , playerCoord = 6
      , playerDir = S
      , width = 5
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
          List.concat
            <| Array.toList (Array.indexedMap (tileTags model) model.level.blueprint)
        )
    , div [] [Html.text <| if model.playerAlive then "" else "Died" ]
    ]

tileTags : Model -> Coord -> Tile -> List (Html Msg)
tileTags model i tag = tileBackground model.level.width i tag ++ tileObjects i model

tileBackground : Int -> Coord -> Tile -> List (Html Msg)
tileBackground widht i tile =
  let
    (px, py) = toCoords widht i

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
        [ imgTile i (10, 10) "atlas" ]
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
        [ imgTile i (7, 12) "atlas" ]
      Obstical _ Pushed ->
        [ imgTile i (8, 12) "atlas" ]
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
  if List.member i model.level.creatures
    then
    [ imgTile i (cycle (0, 8) [(1, 8), (2, 8)] model.animationTick) "atlas" ]
    else if model.level.swordPos == i
      then [ imgTile i (8, 15) "atlas" ]
      else
        if model.level.playerCoord == i
        then [ imgTile i (6, 4) "atlas" ]
        else []

imgTile : Coord -> (Int, Int) -> String -> Html Msg
imgTile i (px, py) file =
  svg
    [ x (String.fromInt ((modBy w i) * 32))
    , y (String.fromInt ((i // w) * 32))
    , width "32"
    , height "32"
    , viewBox (String.fromInt (32 * px) ++ " " ++ String.fromInt (32 * py) ++ " 32 32")
    ]
    [ Svg.image
      [ xlinkHref "/assets/underworld_load/underworld_load-atlas-32x32.png" ] []
    ]
