import Array
import Browser
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
  ( model
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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
    N -> -w
    NE -> 1 - w
    E -> 1
    SE -> w + 1
    S -> w
    SW -> w - 1
    W -> -1
    NW -> -w - 1
