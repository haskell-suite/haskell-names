l1 ax ay az =
  [ (x,y,z)
  | x <- ax
  | y <- ay
  | z <- az
  ]

l2 ax ay az =
  [ (x,y,z)
  | x <- ax x -- neither of these x's should be in scope
  | y <- ay x
  | z <- az x
  ]
