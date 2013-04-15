module FnBind where

f x y = x + y + z + t
  where
    -- NB: mutually recursive bindings
    z = x + t
    t = y + z
