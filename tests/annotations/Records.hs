{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Records where

import qualified Prelude

data A = A { b :: A, c :: A } | X { b :: A, xxx :: A }

undefined = undefined

-- simple record pattern match
f A { b = q } = q

-- simple record update

fu x = x { b = b x }

-- pattern pun
g A {b} = b

-- top-level pattern pun clashes with selector
-- A { c } = undefined
ty = c

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

-- mixed wildcard and normal pattern
f2 A { b = v, .. } = (v, c)

-- mixed wildcard and pun
f3 A { b, .. } = (b, c)

-- simple wildcard construction
r1 = let b = 1 in A { .. }

-- wildcard construction referring to a global and a local values
-- (global values are defined above with a top-level pattern wildcard)
r2 = let selector2 = undefined in Prelude.DataTypeWithSelectors { .. }

-- mixed simple construction and wildcard
r3 = let b = undefined in A { c = undefined, .. }

-- mixed simple construction and pun
r4 = let (b,c) = undefined in A { c, .. }

-- test that we don't bring fields from other constructors
r5 = let (b, xxx) = undefined in A { .. }
