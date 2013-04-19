module ListComp where

a b = [ (x,y,z,t)
      | x <- b
      , y <- x
      , let t = y
      , y <- y
      , t
      , x
      , z <- x
      ]
