{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Records where

import qualified Prelude

data A = A { b :: A, c :: A }

undefined = undefined

-- simple record pattern match
f A { b = q } = q

-- simple record update

fu x = x { b = b x }

-- pattern pun
g A {b} = b

-- top-level pattern pun
A { tx } = undefined
ty = tx

-- expression pun
tz = let b = 1 in A { b }

-- mixed pattern
h (A {b, c = (4, y)}) = (b, y)

-- mixed expression
tm = let b = 1; v = 2 in A { b, c = v }

-- stripping module name
-- (not supported by HSE yet)
{- f A { M.b } = b -}

-- simple wildcard pattern
f A { .. } = (b, c)

-- top-level wildcard pattern
Prelude.DataTypeWithSelectors { .. } = undefined
aux = (selector1,selector2)
