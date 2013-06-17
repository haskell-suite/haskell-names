module E7 (Foo(..), Bar(..), N(..)) where

data Foo = Foo1 | Foo2 Int | Foo3 { c :: Bool }

class Bar where x :: Foo

newtype N = N { unN :: Foo }

a = a

data Baz = Baz
