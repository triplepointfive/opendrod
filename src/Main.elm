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

type alias DArray a = Array.Array (Array.Array a)

type Tile = Wall | Floor

type alias Model = DArray Tile

-- HTML

type Msg = KeyPress String

init : () -> (Model, Cmd.Cmd Msg)
init () =
  ( Array.fromList
    <| List.map Array.fromList
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
  , Cmd.none
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( model
  , Cmd.none
  )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
  (List.concat <|
  Array.toList <|
  Array.indexedMap (\i row -> Array.toList <| Array.indexedMap (tileTag i) row) model)
  |>
    svg [ width "500", height "500", viewBox "0 0 500 500" ]

tileTag : Int -> Int -> Tile -> Html Msg
tileTag j i tag =
  let symbol = if tag == Floor then " " else "#"
  in
    text_
      [ x (String.fromInt (i * 10))
      , y (String.fromInt ((1 + j) * 12))
      , fill "red"
      ]
      [ Svg.text symbol
      ]
