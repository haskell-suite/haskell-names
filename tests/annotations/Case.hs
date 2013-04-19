module Case where

-- TODO: test where, guards

f x = case x of
  Right (a,b,c) -> a b c
  Left 0 -> 0
  Left y
    | u -> u
    | let v = u, k <- v -> u
    where
      u = y
      q = v -- not in scope
  Left y -> t a y -- NB: 'a' is intentionally unbound
    where
      t = y x
