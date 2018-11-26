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
import Dir exposing (..)
import Game exposing (..)
import Room exposing (..)
import Level
import Utils exposing (..)

import Levels.Level1

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
  { game : Game
  , animationTick : Int
  , justLoaded : Bool
  , effect : Maybe Effect
  }

type Effect = TileClicked Tile | ChangeRoom Room Float Point

type Image = Atlas | BaseMeph | Constructions | Lomem

type Msg = KeyPress String | Tick | Click Tile | AnimationRate Float

init : () -> (Model, Cmd.Cmd Msg)
init () =
  let
    currentRoom = Levels.Level1.level.init
  in
    ( { game =
        { room = currentRoom
        , alive = True
        , backsteps = []
        , checkpoints = [currentRoom]
        , level = Levels.Level1.level
        }
      , justLoaded = False
      , effect = Nothing
      , animationTick = 0
      }
    , Cmd.none
    )

onGame : (Game -> Game) -> Model -> Model
onGame f model = { model | game = f model.game }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick -> ( tick model, Cmd.none )
    AnimationRate delta -> ( tickEffect delta model, Cmd.none )

    KeyPress "Backspace" -> ( undo model, Cmd.none )
    KeyPress "r" -> ( loadCheckpoint model, Cmd.none )

    KeyPress "q" -> ( onGame (Game.turn Dir.left) model, Cmd.none )
    KeyPress "w" -> ( onGame (Game.turn Dir.right) model, Cmd.none )

    KeyPress "j" -> ( onGame (Game.move S) model, Cmd.none )
    KeyPress "k" -> ( onGame (Game.move N) model, Cmd.none )
    KeyPress "l" -> ( onGame (Game.move E) model, Cmd.none )
    KeyPress "h" -> ( onGame (Game.move W) model, Cmd.none )

    KeyPress "y" -> ( onGame (Game.move NW) model, Cmd.none )
    KeyPress "u" -> ( onGame (Game.move NE) model, Cmd.none )
    KeyPress "b" -> ( onGame (Game.move SW) model, Cmd.none )
    KeyPress "n" -> ( onGame (Game.move SE) model, Cmd.none )

    KeyPress _ -> ( model, Cmd.none )

    Click tile -> ( addClickTileEffect tile model, Cmd.none )

undo : Model -> Model
undo model = { model | game = Game.undo model.game }

loadCheckpoint : Model -> Model
loadCheckpoint model =
  case model.game.checkpoints of
    x :: xs ->
      { model
      | game =
        { room = x
        , alive = True
        , backsteps = []
        , checkpoints = xs
        , level = model.game.level
        }
      , justLoaded = True
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
          , justLoaded = False
          , game =
            { room = room
            , backsteps = []
            , checkpoints = [room]
            , alive = True
            , level = model.game.level
            }
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
          model.game.room
      _ ->
        drawRoom (0, 0) model.game.room
    , lazy (drawMinimap) model.game.level
    , div [] [Html.text <| if model.game.alive then "" else "Died" ]
    ]

drawMinimap : Level.Level -> Html Msg
drawMinimap level =
  svg
    [ width "228"
    , height "192"
    , viewBox "0 0 152 128"
    ]
    <| (rect [ x "0" , y "0" , width "152" , height "128" , fill "rgb(75, 73, 75)" ] [])
        :: (List.concat <| Dict.values <| Dict.map (drawMinimapRoom level.currentRoomId) level.rooms)
        ++ [rect [ x "57", y "48", width "38", height "32", stroke "gold", strokeWidth "3", fill "none" ] []]
        ++ [rect [ x "0", y "0", width "152", height "128", stroke "gold", strokeWidth "5", fill "none" ] []]

drawMinimapRoom : Room.RoomId -> Room.RoomId -> Level.RoomRep -> List (Html Msg)
drawMinimapRoom (ox, oy) (dx, dy) room =
  if room.state == Level.Unseen
    then []
    else
      let
        (baseX, baseY) =
          ( (152 - 38) // 2 + 38 * (dx - ox)
          , (128 - 32) // 2 + 32 * (dy - oy)
          )
      in
      rect
        [ x <| String.fromInt baseX
        , y <| String.fromInt baseY
        , width "38"
        , height "32"
        , fill <| case room.state of
          Level.Unseen -> "none"
          Level.Complete -> "white"
          Level.Seen -> "red"
        ] []
        :: List.concat (List.indexedMap
          (\i tile ->
            let
              (tdx, tdy) = toPoint 38 i
            in
            if tile
              then [
                rect
                  [ x <| String.fromInt <| baseX + tdx
                  , y <| String.fromInt <| baseY + tdy
                  , width "1"
                  , height "1"
                  , fill "black"
                  ]
                  []
                ]
              else []
          )
          room.minimap
          )

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
    (px, py) = toPoint room.width i
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
      GreenDoor _ -> background -- TODO: Draw door here

    tileSet = case tile of
      Wall -> Constructions
      _ -> Lomem

    tileItems = case tile of
      Floor -> []
      Wall -> []
      GreenDoor Open -> []
      GreenDoor Closed -> [ imgTile displayPos (12, 8) Constructions ]
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
  let p = Utils.sub (toPoint room.width i) offset
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
