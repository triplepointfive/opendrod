module Utils exposing (..)

type alias Point = (Int, Int)

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

toCoords : Int -> Int -> Point
toCoords w i = (modBy w i , i // w)

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
