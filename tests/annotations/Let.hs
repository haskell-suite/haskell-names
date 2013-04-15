module Let where

a =
  let x = 2
      y = 3
  in let
      z = 4
      y = 2
  in x + y + z
