module ClassInstances where

data D a = D DataType a

class C a where
    wiggle :: a -> a
    woe :: a
    method1 :: a
    ($$$) :: a -> a -> a

instance (C a) => C (D a) where
    wiggle (D b a) = D b (f a)
    woe = D Constructor1 woe
    method1 = D Constructor1 woe
    f $$$ x = ($$$) f x

f :: (C a) => a -> a
f x = wiggle x
