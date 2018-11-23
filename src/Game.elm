module Game exposing (..)

import Dir
import Room exposing (..)
import Utils exposing (..)

playerMoveDir : Dir.Dir -> Room -> MoveResult
playerMoveDir dir level =
  let
    destPos = Dir.moveCoord level.width level.playerCoord dir
    (px, py) = toCoords level.width level.playerCoord
    (x, y) = Dir.move dir (px, py)
    (lx, ly) = Dir.delta dir
  in
    if canPlayerMoveTo level.playerCoord level destPos
    then Move { level | playerCoord = destPos }
    else if x >= 0 && y >= 0 && x < level.width && y < level.height
      then Move level
      else
        let delta = if x < 0 || x >= level.width then (lx, 0) else (0, ly) in
        Leave
          delta

          ( modBy level.width (px + fst delta)
          , modBy level.height (py + snd delta)
          )
