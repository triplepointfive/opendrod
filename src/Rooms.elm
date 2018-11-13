module Rooms exposing (..)

import Array
import Dict

import Room exposing (..)
import Utils exposing (Point)

level1 : Point -> Dir -> Room
level1 (px, py) dir =
  let blueprint =
        buildBlueprint
          [ "######################################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "############       ###################"
          , "############       ###################"
          , "############     a ###################"
          , "############       ###################"
          , "############     b ###################"
          , "############       ###################"
          , "############     c ###################"
          , "############       ###################"
          , "############       ###################"
          , "##############   #####################"
          , "##############AAA#####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############BBB#####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############CCC#####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          ]
          [ ('a', Orb [(0, Open)])
          , ('b', Orb [(1, Toggle), (2, Toggle)])
          , ('c', Orb [(2, Close)])
          , ('A', Obstical 0 Pushed)
          , ('B', Obstical 1 InGround)
          , ('C', Obstical 2 Pushed)
          ]
  in
  { blueprint = blueprint
  , creatures = []
  , width = 38
  , height = 32
  , pos = (0, 0)

  , swordPos = dirCoord 38 (px + py * 38) dir
  , playerCoord = (px + py * 38)
  , playerDir = dir
  , wallTiles = buildWalls blueprint 38
  }

level2 : Point -> Dir -> Room
level2 (px, py) dir =
  let blueprint =
        buildBlueprint
          [ "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "############       ###################"
          , "############ ##### ###################"
          , "############       ###################"
          , "############       ###################"
          , "##############   #####################"
          , "############       ###################"
          , "############ ##### ###################"
          , "############ ##### ###################"
          , "############       ###################"
          , "############       ###################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #####################"
          , "##############   #########            "
          , "##############   #########            "
          , "##############   #########            "
          , "##############   #########   #########"
          , "##############   #########   #########"
          , "##############   #########   #########"
          ]
          []
  in
  { blueprint = blueprint
  , creatures = [737]
  , width = 38
  , height = 32
  , pos = (0, 1)

  , swordPos = dirCoord 38 (px + py * 38) dir
  , playerCoord = (px + py * 38)
  , playerDir = dir
  , wallTiles = buildWalls blueprint 38
  }

level3 : Point -> Dir -> Room
level3 (px, py) dir =
  let blueprint =
        buildBlueprint
          [ "##############   #########   #########"
          , "#############   ##########   #########"
          , "############   ###########   #########"
          , "###########   ############   #########"
          , "##########   #############   #########"
          , "##########   #############   #########"
          , "##########   #############   #########"
          , "##########   #############   #########"
          , "#########   ##############   #########"
          , "########   ###############   #########"
          , "#####         #########         ######"
          , "#####         #########         ######"
          , "#####  N J B  #########  NJJJB  ######"
          , "#####                    L   H  ######"
          , "#####  L   H         X   L   H  ######"
          , "#####                    L   H  ######"
          , "#####  U K Y  #########  UKKKY  ######"
          , "#####         #########         ######"
          , "#####         #########         ######"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          , "######################################"
          ]
          [ ('Y', Arrow NW)
          , ('U', Arrow NE)
          , ('H', Arrow W)
          , ('L', Arrow E)
          , ('J', Arrow S)
          , ('K', Arrow N)
          , ('B', Arrow SW)
          , ('N', Arrow SE)
          ]
  in
  { blueprint = blueprint
  , creatures = [559]
  , width = 38
  , height = 32
  , pos = (0, 2)

  , swordPos = dirCoord 38 (px + py * 38) dir
  , playerCoord = (px + py * 38)
  , playerDir = dir
  , wallTiles = buildWalls blueprint 38
  }

buildBlueprint : List String -> List (Char, Tile) -> Array.Array Tile
buildBlueprint walls tiles =
  let
    tilesMap = Dict.fromList tiles

    buildTile i char =
      case Dict.get char tilesMap of
        Just t -> t
        Nothing -> case char of
          '#' -> Wall
          'X' -> Checkpoint
          _ -> Floor
  in
  Array.indexedMap buildTile <| Array.fromList <| List.concatMap String.toList walls

buildWalls : Array.Array Tile -> Int -> Array.Array (Int, Int)
buildWalls blueprint width =
  let
    layer = 0

    tileToWall coord tile =
      let
        w dir =
          if modBy width coord == 0 && List.member dir [W, NW, SW] then True
          else Maybe.withDefault Wall (Array.get (dirCoord width coord dir) blueprint) == Wall
      in
      case tile of
        Wall ->
          let walls = [w N, w NE, w E, w SE, w S, w SW, w W, w NW] in
          case walls of
            -- Fully surrounded
            [True, True, True, True, True, True, True, True] -> (-1, -1)

            -- Corners
            [True, False, True, True, True, True, True, True] -> (2, layer)
            [True, True, True, False, True, True, True, True] -> (3, layer)
            [True, True, True, True, True, False, True, True] -> (4, layer)
            [True, True, True, True, True, True, True, False] -> (5, layer)

            -- Corners
            [True, True, True, True, False, False, False, False] -> (2, layer)
            [False, True, True, True, True, False, False, False] -> (3, layer)
            [False, False, False, False, True, True, True, True] -> (4, layer)
            [True, False, False, False, False, True, True, True] -> (5, layer)

            -- Corners
            [True, True, True, False, False, False, False, False] -> (2, layer)
            [False, False, True, True, True, False, False, False] -> (3, layer)
            [False, False, False, False, True, True, True, False] -> (4, layer)
            [True, False, False, False, False, False, True, True] -> (5, layer)

            -- Horizontal
            [True, True, True, _, False, _, True, True] -> (0, layer)
            [False, _, True, True, True, True, True, _] -> (0, layer)
            [False, _, True, _, False, _, True, _] -> (0, layer)

            -- Vertical
            [True, _, False, _, True, True, True, True] -> (1, layer)
            [True, True, True, True, True, _, False, _] -> (1, layer)
            [True, _, False, _, True, _, False, _] -> (1, layer)

            -- T blocks
            [True, False, True, False, True, True, True, True] -> (6, layer)
            [True, True, True, True, True, False, True, False] -> (7, layer)
            [False, _, True, _, True, _, True, _] -> (8, layer)
            [True, _, True, _, False, _, True, _] -> (9, layer)

            -- Corners
            [True, _, True, _, False, _, False, _] -> (2, layer)
            [False, _, True, _, True, _, False, _] -> (3, layer)
            [False, _, False, _, True, _, True, _] -> (4, layer)
            [True, _, False, _, False, _, True, _] -> (5, layer)

            _ -> if (List.length <| List.filter w [N, E, S, W]) == 1
              then (12, layer) else (3, 4)
        _ -> (0, 0)
  in
    Array.indexedMap tileToWall blueprint
