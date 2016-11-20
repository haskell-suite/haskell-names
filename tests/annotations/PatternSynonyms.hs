{-# LANGUAGE PatternSynonyms, NamedFieldPuns #-}

module PatternSynonyms where


data Type = App String [Type]

pattern Arrow t1 t2 = App "->" [t1, t2]
pattern Int = App "Int" []
pattern Maybe t = App "Maybe" [t]

collectArgs :: Type -> [Type]
collectArgs (Arrow t1 t2) = t1 : collectArgs t2
collectArgs _             = []

isInt :: Type -> Bool
isInt Int = True
isInt _   = False

isIntEndo :: Type -> Bool
isIntEndo (Arrow Int Int) = True
isIntEndo _               = False

intEndo :: Type
intEndo = Arrow Int Int

pattern Head x <- x:xs

pattern HeadC x <- x:xs where
  HeadC x = [x]

pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)

zero = Point 0 0
zero' = Point { x = 0, y = 0}
isZero (Point 0 0) = True
isZero' (Point { x = 0, y = 0 }) = True
getX (Point {x}) = x
setX = (0, 0) { x = 1 } == (1,0)
getX' = x (0,0) == 0

