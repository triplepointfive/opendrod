module Levels.Level1 exposing (level)

import Dict
import Debug

import Dir
import Level exposing (..)
import Room exposing (..)
import Utils exposing (Point)

level : Level.Level
level =
  { init = buildRoom False (14, 15) Dir.E roomm1m1
  , currentRoomId = (-1, -1)
  , complete = False
  , rooms = Dict.fromList
    [ ( (-1, -1)
      , { state = Level.Complete
        , secret = False
        , builder = roomm1m1
        , minimap = buildMinimap roomm1m1
        }
      )
    , ( (0, -1)
      , { state = Level.Unseen
        , secret = False
        , builder = room0m1
        , minimap = buildMinimap room0m1
        }
      )
    , ( (0, 0)
      , { state = Level.Unseen
        , secret = False
        , builder = room00
        , minimap = buildMinimap room00
        }
      )
    , ( (0, 1)
      , { state = Level.Unseen
        , secret = False
        , builder = room01
        , minimap = buildMinimap room01
        }
      )
    , ( (0, 2)
      , { state = Level.Unseen
        , secret = False
        , builder = room02
        , minimap = buildMinimap room02
        }
      )
    , ( (1, 1)
      , { state = Level.Unseen
        , secret = False
        , builder = room11
        , minimap = buildMinimap room11
        }
      )
    ]
  }

roomm1m1 : RoomBuilder
roomm1m1 =
  { blueprint =
    [ "######################################"
    , "######################################"
    , "######################################"
    , "######################################"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                                  "
    , "####                                  "
    , "####                                  "
    , "####        RRRRRR                    "
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "####                              ####"
    , "######################################"
    , "######################################"
    , "######################################"
    , "######################################"
    ]
  , repository =
    [ ('R', Creature)
    ]
  }

room0m1 : RoomBuilder
room0m1 =
  { blueprint =
    [ "######################################"
    , "######################################"
    , "#################               T#####"
    , "#################                #####"
    , "#################                #####"
    , "#################                #####"
    , "#################    ########    #####"
    , "#################    ########    #####"
    , "#################    ########    #####"
    , "#################    ########    #####"
    , "#################    ########    #####"
    , "#################    ########    #####"
    , "#################    ########    #####"
    , "#################CCCC########    #####"
    , "                O    O           #####"
    , "                O    O           #####"
    , "                O    O           #####"
    , "                O    O           #####"
    , "# ###############CCCC#################"
    , "## ##############    #################"
    , "### #############    #################"
    , "#### ############    #################"
    , "##### ###########   T#################"
    , "###### ##########    #################"
    , "####### #########    #################"
    , "######## ########    #################"
    , "######### #######    #################"
    , "########## ######    #################"
    , "########### #####    #################"
    , "############ ####    #################"
    , "#############        #################"
    , "#################    #################"
    ]
  , repository =
    [ ('T', Tile <| Orb [(0, Toggle)])
    , ('C', Tile <| Door 0 Closed)
    , ('O', Tile <| Door 0 Open)
    ]
  }

room00 : RoomBuilder
room00 =
  { blueprint =
    [ "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "###############        ###############"
    , "###############        ###############"
    , "###############      a ###############"
    , "###############        ###############"
    , "###############      b ###############"
    , "###############        ###############"
    , "###############      c ###############"
    , "###############        ###############"
    , "###############        ###############"
    , "#################    #################"
    , "#################AAAA#################"
    , "#################    #################"
    , "#################    #################"
    , "#################BBBB#################"
    , "#################    #################"
    , "#################    #################"
    , "#################CCCC#################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    , "#################    #################"
    ]
  , repository =
    [ ('a', Tile <| Orb [(0, ToOpen)])
    , ('b', Tile <| Orb [(1, Toggle), (2, Toggle)])
    , ('c', Tile <| Orb [(2, Close)])
    , ('A', Tile <| Door 0 Closed)
    , ('B', Tile <| Door 1 Open)
    , ('C', Tile <| Door 2 Closed)
    ]
  }

room01 : RoomBuilder
room01 =
  { blueprint =
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
    , "############## R #####################"
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
  , repository =
    [ ('R', Creature)
    ]
  }

room02 : RoomBuilder
room02 =
  { blueprint =
      [ "##############   #########   #########"
      , "#############   ##########   #########"
      , "############   ###########   #########"
      , "###########   ############   #########"
      , "##########   #############   #########"
      , "##########   #############   #########"
      , "##########   #############   #########"
      , "##########   #############   #########"
      , "#########   ##############   #########"
      , "########   ###############GGG#########"
      , "#####         #########         ######"
      , "#####         #########         ######"
      , "#####  N J B  #########  NJJJB  ######"
      , "#####                    L   H  ######"
      , "#####  L   H         X   L R H  ######"
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
  , repository =
    [ ('R', Creature)
    , ('Y', Tile <| Arrow Dir.NW)
    , ('U', Tile <| Arrow Dir.NE)
    , ('H', Tile <| Arrow Dir.W)
    , ('L', Tile <| Arrow Dir.E)
    , ('J', Tile <| Arrow Dir.S)
    , ('K', Tile <| Arrow Dir.N)
    , ('B', Tile <| Arrow Dir.SW)
    , ('N', Tile <| Arrow Dir.SE)
    , ('G', Tile <| GreenDoor Closed)
    ]
  }

room11 : RoomBuilder
room11 =
  { blueprint =
    [ "######################################"
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
    , "       ###############################"
    , "       ###############################"
    , "       ###############################"
    , "######################################"
    , "######################################"
    , "######################################"
    ]
  , repository =
    []
  }
