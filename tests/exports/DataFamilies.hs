{-# LANGUAGE TypeFamilies #-}
module DataFamilies (
    Vector,
    ListLike(..)) where

data family Vector a

class ListLike a where
  type I a
  h :: a -> I a

newtype instance Vector () = U_Vector ()
