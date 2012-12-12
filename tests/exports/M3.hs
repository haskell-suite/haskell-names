module M3 where

data A = B { c :: A }

newtype G = H A

type L = G
