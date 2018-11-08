module Utils exposing (..)

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

toCoords : Int -> Int -> (Int, Int)
toCoords w i = (modBy w i , i // w)
