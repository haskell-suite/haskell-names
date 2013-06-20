module SimpleTypes where

data A = B { c :: A }

newtype G = H A

type L = G
