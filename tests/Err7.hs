-- Duplicate selectors
data T = T { x :: Int }
data S = S { x :: Int }

data U = U { y :: Int, y :: Int }

-- But this OK
--data X = X1 { a :: Int } | X2 { a :: Int }
