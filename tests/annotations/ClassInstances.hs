module ClassInstances where

data D a = D Bool a

class C a where
    wiggle :: a -> Bool

instance (C a) => C (D a) where
    wiggle (D b _) = b

f :: (C a) => a -> Bool
f x = wiggle x
