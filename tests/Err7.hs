-- Duplicate selectors
data T = T { x :: Int }
data S = S { x :: Int }

data U = U { y :: Int, y :: Int }  -- XXX error not detected yet

-- But this OK
data X = X1 { a :: Int } | X2 { a :: Int }

class C a where
    m :: a
    m :: a
