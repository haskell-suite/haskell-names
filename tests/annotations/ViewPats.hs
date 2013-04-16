{-# LANGUAGE ViewPatterns #-}
module ViewPats where

f (y -> x) z@(x -> y) = do
  x
  y
  z
