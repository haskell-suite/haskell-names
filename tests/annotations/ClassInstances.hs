module ClassInstances where

data D a = D Bool a

class C a where
    wiggle :: a -> a
    woe :: a
    ($$$) :: a -> a -> a

instance (C a) => C (D a) where
    wiggle (D b a) = D b (f a)
    woe = D False woe
    f $$$ x = ($$$) f x

f :: (C a) => a -> a
f x = wiggle x
