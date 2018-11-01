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

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

type alias Coord = Int

type Tile = Wall | Floor

type alias Creature = Coord

type alias Model =
  { blueprint : Array.Array Tile
  , creatures : List Creature
  , swordPos : Coord
  , playerCoord : Coord
  , playerDir : Dir
  , playerAlive : Bool
  , animationTick : Int
  }

type Dir = N | NE | E | SE | S | SW | W | NW

-- HTML

type Msg = KeyPress String | Tick

w = 4

init : () -> ( Model, Cmd.Cmd Msg )
init () =
  ( { blueprint =
    Array.fromList
      <| List.concat
        [ [Wall, Wall, Wall, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Floor, Floor, Wall]
        , [Wall, Wall, Wall, Wall]
        ]
    , creatures = [29, 30, 33, 34, 25, 26]
    , swordPos = 6
    , playerCoord = 5
    , playerDir = E
    , playerAlive = True
    , animationTick = 0
    }
  , Cmd.none
  )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick -> ( tick model , Cmd.none )

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

tick : Model -> Model
tick model =
  { model
  | animationTick = model.animationTick + 1
  }

withAction : (Model -> Model) -> Model -> Model
withAction action model =
  let actualModel = checkSword <| isAlivePlayer <| buildSwordPos <| action model
  in
    List.foldl
      (\creature md -> isAlivePlayer <| creatureTurn creature md)
      actualModel
      <| List.sortBy (squareDistanceToPlayer actualModel) actualModel.creatures

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
          directMoves =
            List.map ((+) coord)
              <| List.reverse
              <| List.sortBy abs
              <| [dx + dy, dy, dx]
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

playerMoveDir : Dir -> Model -> Model
playerMoveDir dir model =
  let destPos = dirCoord model.playerCoord dir
  in
    if canPlayerMoveTo destPos model
    then { model | playerCoord = destPos }
    else model

canMoveTo : Coord -> Model -> Bool
canMoveTo coord model =
  canPlayerMoveTo coord model && model.swordPos /= coord

canPlayerMoveTo : Coord -> Model -> Bool
canPlayerMoveTo coord model =
  case Array.get coord model.blueprint of
    Nothing -> False
    Just Floor -> List.isEmpty <| List.filter ((==) coord) model.creatures
    Just Wall -> False
      -- TODO: Check nobody is here

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
    [ div [] [Html.text <| if model.playerAlive then "" else "Died"
    , svg
        [ width "500", height "500", viewBox "0 0 500 500" ]
        (
          List.concat
            <| Array.toList (Array.indexedMap (tileTags model) model.blueprint)
        )
      ]
    ]

tileTags : Model -> Coord -> Tile -> List (Html Msg)
tileTags model i tag =
  tileBackground i tag ++ tileObjects i model

tileBackground : Coord -> Tile -> List (Html Msg)
tileBackground i tile =
  let
    pos = case tile of
      Floor -> "64 224 32 32"
      Wall -> "0 32 32 32"
  in
    [ svg
      [ x (String.fromInt ((modBy w i) * 32))
      , y (String.fromInt ((i // w) * 32))
      , width "32"
      , height "32"
      , viewBox pos
      ]
      [ Svg.image [ xlinkHref "/underworld_load/underworld_load-lomem-32x32.png" ] []]
    ]

tileObjects : Coord -> Model -> List (Html Msg)
tileObjects i model =
  let
    atlas pos =
      svg
        [ x (String.fromInt ((modBy w i) * 32))
        , y (String.fromInt ((i // w) * 32))
        , width "32"
        , height "32"
        , viewBox pos
        ]
        [ Svg.image [ xlinkHref "/underworld_load/underworld_load-atlas-32x32.png" ] []]
  in
    if List.member i model.creatures
       then [ atlas <| ordered "0 256 32 32" ["32 256 32 32", "64 256 32 32"] model.animationTick ]
       else if model.swordPos == i
         then [ atlas "256 480 32 32" ]
         else
           if model.playerCoord == i
           then [ atlas "192 128 32 32" ]
           else []

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

toCoords : Int -> (Int, Int)
toCoords i = (modBy w i , i // w)

squareDistanceToPlayer : Model -> Creature -> Int
squareDistanceToPlayer model coords =
  let
    (px, py) = toCoords model.playerCoord
    (x, y) = toCoords coords
    (dx, dy) = (px - x, py - y)
  in
    dx * dx + dy * dy

-- Utils

ordered : a -> List a -> Int -> a
ordered x xs i =
  case List.drop (modBy (1 + List.length xs) i) xs of
    a :: _ -> a
    _ -> x

sign : Int -> Int
sign x =
  if x > 0
    then 1
    else if x < 0
      then -1
      else 0

const : a -> b -> a
const x _ = x
