module ExportListMembers (Foo(Foo1, Foo3, c), Bar(x), N(N)) where

data Foo = Foo1 | Foo2 { c :: Bool } | Foo3 { d :: Bool }

class Bar where x :: Foo; y :: Foo

newtype N = N { unN :: Foo }

a = a

data Baz = Baz
