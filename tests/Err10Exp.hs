module Err10Exp where

data T = C1 { x :: Int } | C2 { y :: Bool }

class C a where
    m1 :: a -> Int

foo :: Int
foo = 10

