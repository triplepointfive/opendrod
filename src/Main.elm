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

type alias Creature = Coord

type alias Model =
  { blueprint : DArray Tile
  , creatures : List Creature
  , swordPos : Coord
  , playerCoord : Coord
  , playerDir : Dir
  , playerAlive : Bool
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
    , creatures = [34]
    , swordPos = 6
    , playerCoord = 5
    , playerDir = E
    , playerAlive = True
    }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyPress "q" ->
      ( withAction (turn dirLeft) model
      , Cmd.none
      )
    KeyPress "w" ->
      ( withAction (turn dirRight) model
      , Cmd.none
      )

    KeyPress "j" ->
      ( withAction (moveDir S) model, Cmd.none )
    KeyPress "k" ->
      ( withAction (moveDir N) model, Cmd.none)
    KeyPress "l" ->
      ( withAction (moveDir E) model, Cmd.none )
    KeyPress "h" ->
      ( withAction (moveDir W) model, Cmd.none)

    KeyPress "y" ->
      ( withAction (moveDir NW) model, Cmd.none )
    KeyPress "u" ->
      ( withAction (moveDir NE) model , Cmd.none)
    KeyPress "b" ->
      ( withAction (moveDir SW) model, Cmd.none )
    KeyPress "n" ->
      ( withAction (moveDir SE) model , Cmd.none)

    _ ->
      ( model
      , Cmd.none
      )

withAction : (Model -> Model) -> Model -> Model
withAction action model =
  let actualModel = checkSword <| isAlivePlayer <| buildSwordPos <| action model
  in
    List.foldl
      (\creature md -> isAlivePlayer <| creatureTurn creature md)
      actualModel
      actualModel.creatures

creatureTurn : Creature -> Model -> Model
creatureTurn creature model =
  if model.playerAlive
    then roachAI creature model
    else model

roachAI : Creature -> Model -> Model
roachAI coord model =
  if List.member coord model.creatures
    then
      let dx = (modBy w model.playerCoord) - (modBy w coord)
          dy = w * sign ((model.playerCoord // w) - (coord // w))
          directMoves = List.map ((+) coord) <| List.reverse <| List.sortBy abs <| [dx + dy, dy, dx]
          newCoord =
            case List.filter (\ i -> canMoveTo i model) directMoves of
              freeCoord :: _ -> freeCoord
              _ -> coord
      in
      { model
      | creatures = newCoord :: List.filter ((/=) coord) model.creatures
      }
    else
      model

buildSwordPos : Model -> Model
buildSwordPos model =
  { model
  | swordPos = dirCoord model.playerCoord model.playerDir
  }

checkSword : Model -> Model
checkSword model =
  { model
  | creatures = List.filter ((/=) model.swordPos) model.creatures
  }

isAlivePlayer : Model -> Model
isAlivePlayer model =
  { model
  | playerAlive = List.all ((/=) model.playerCoord) model.creatures
  }

turn : (Dir -> Dir) -> Model -> Model
turn newDir model =
  { model | playerDir = newDir model.playerDir }

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
  div []
    [ div [] [Html.text <| if model.playerAlive then "" else "Died"
    , svg
        [ width "500", height "500", viewBox "0 0 500 500" ]
        (
          Array.toList (Array.indexedMap (tileTag model) model.blueprint)
        )
      ]
    ]

tileTag : Model -> Coord -> Tile -> Html Msg
tileTag ({ swordPos, playerCoord, playerDir, creatures }) i tag =
  let
    symbol
      = if List.member i creatures
        then "0"
        else if swordPos == i
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

sign : Int -> Int
sign x =
  if x > 0
    then 1
    else if x < 0
      then -1
      else 0

toCoords : Int -> (Int, Int)
toCoords i = (modBy w i , i // w)
