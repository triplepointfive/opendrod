module Utils exposing (..)

type alias Point = (Int, Int)

type alias Coord = Int

cycle : a -> List a -> Int -> a
cycle x xs i =
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

even : Int -> a -> a -> a
even i t f =
  if modBy 2 i == 0 then t else f

toPoint : Int -> Int -> Point
toPoint w i = (modBy w i , i // w)

toCoord : Point -> Int -> Int
toCoord (x, y) w = w * y + x

fst : (a, b) -> a
fst (a, _) = a

snd : (a, b) -> b
snd (_, b) = b

add : Point -> Point -> Point
add (x, y) (dx, dy) = (x + dx, y + dy)

sub : Point -> Point -> Point
sub (x, y) (dx, dy) = (x - dx, y - dy)

pair : a -> b -> (a, b)
pair a b = (a, b)

canDo : (b -> Bool) -> (b -> a) -> b -> Maybe a
canDo flag f v = if flag v then Just (f v) else Nothing

either : (a -> b) -> (a -> b) -> (a -> Bool) -> a -> b
either l r c v = if c v then r v else l v
