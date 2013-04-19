module FnBind where

f x y = x y z t
  where
    -- NB: mutually recursive bindings
    z = x t
    t = y z
f x y |
  let z = x y,
  (k1, k2) <- z,
  k1,
  k2
  = y

(z * x) y = x y z t
  where
    t = y z
