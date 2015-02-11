{-# LANGUAGE TypeFamilies #-}
module DataFamilies (
    ListLike(..),
    Vector(..)) where

data family Vector a

class ListLike a where
  type I a
  h :: a -> I a

newtype instance Vector () = U_Vector ()
