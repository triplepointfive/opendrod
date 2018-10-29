import Array
import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias DArray a = Array.Array a

type alias Coord = Int

type Tile = Wall | Floor

type alias Model =
  { blueprint : DArray Tile
  , creatures : List Coord
  , playerCoord : Coord
  , playerDir : Dir
  }

type Dir = N | NE | E | SE | S | SW | W | NW

-- HTML

type Msg = KeyPress String

w = 4

init : () -> ( Model, Cmd.Cmd Msg )
init () =
  ( { blueprint =
    Array.fromList
    <| List.concat
    [ [Wall, Wall, Wall, Wall]
    , [Wall, Floor, Floor, Wall]
    , [Wall, Floor, Floor, Wall]
    , [Wall, Floor, Wall, Wall]
    , [Wall, Floor, Floor, Wall]
    , [Wall, Floor, Floor, Wall]
    , [Wall, Wall, Floor, Wall]
    , [Wall, Floor, Floor, Wall]
    , [Wall, Floor, Floor, Wall]
    , [Wall, Wall, Wall, Wall]
    ]
    , creatures = []
    , playerCoord = 5
    , playerDir = S
    }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyPress "w" ->
      ( { model | playerDir = turnLeft model.playerDir }
      , Cmd.none
      )
    KeyPress "e" ->
      ( { model | playerDir = turnRight model.playerDir }
      , Cmd.none
      )

    KeyPress "j" ->
      ( moveDir S model, Cmd.none )
    KeyPress "k" ->
      ( moveDir N model , Cmd.none)
    KeyPress "l" ->
      ( moveDir E model, Cmd.none )
    KeyPress "h" ->
      ( moveDir W model , Cmd.none)

    KeyPress "y" ->
      ( moveDir NW model, Cmd.none )
    KeyPress "u" ->
      ( moveDir NE model , Cmd.none)
    KeyPress "b" ->
      ( moveDir SW model, Cmd.none )
    KeyPress "n" ->
      ( moveDir SE model , Cmd.none)

    _ ->
      ( model
      , Cmd.none
      )

moveDir : Dir -> Model -> Model
moveDir dir model =
  let destPos = dirCoord model.playerCoord dir
  in
    if canMoveTo destPos model
    then { model | playerCoord = destPos }
    else model

canMoveTo : Coord -> Model -> Bool
canMoveTo coord model =
  case Array.get coord model.blueprint of
    Nothing -> False
    Just Floor -> True
    Just Wall -> False
      -- TODO: Check nobody is here

subscriptions : Model -> Sub Msg
subscriptions model =
  onKeyDown keyDecoder

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map KeyPress
    <| Decode.field "key" Decode.string

view : Model -> Html Msg
view model =
  svg
    [ width "500", height "500", viewBox "0 0 500 500" ]
    (
      Array.toList (Array.indexedMap (tileTag model) model.blueprint)
    )

tileTag : Model -> Coord -> Tile -> Html Msg
tileTag ({ playerCoord, playerDir }) i tag =
  let
    blade = dirCoord playerCoord playerDir
    symbol
      = if blade == i
        then "-"
        else
          if playerCoord == i
          then "@"
          else
            if tag == Floor
            then " "
            else "＃"
  in
    text_
      [ x (String.fromInt ((modBy w i) * 20))
      , y (String.fromInt ((1 + i // w) * 20))
      , fill "red"
      ]

      [ Svg.text symbol
      ]

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

turnLeft : Dir -> Dir
turnLeft dir =
  case dir of
    N  -> NW
    NE -> N
    E  -> NE
    SE -> E
    S  -> SE
    SW -> S
    W  -> SW
    NW -> W

turnRight : Dir -> Dir
turnRight dir =
  case dir of
    N  -> NE
    NE -> E
    E  -> SE
    SE -> S
    S  -> SW
    SW -> W
    W  -> NW
    NW -> N
