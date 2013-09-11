module ExportList2 (b, c) where

data A
  = A { b :: A }
  | B { b :: A, d :: A }

c = b
