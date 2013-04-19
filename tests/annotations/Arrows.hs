{-# LANGUAGE Arrows #-}
module Arrows where

x y = proc z -> do
  a <- y -< z
  b <- y -< a
  y -< b
