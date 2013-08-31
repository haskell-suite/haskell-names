{-# LANGUAGE NPlusKPatterns #-}
module Pats where

data R = R { y :: R }

f x = x
f (x+1) = x
f (R x) = x
f (x : y) = x
f (R { y = x }) = x
f x@_ = x
