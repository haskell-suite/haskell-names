{-# LANGUAGE MultiParamTypeClasses #-}
module Types where

data A
  = B A
  | C A
  | D { d :: A, e :: A }

type X = A

class C a where
  f :: X -> A
