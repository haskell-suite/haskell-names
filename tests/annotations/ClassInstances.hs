module ClassInstances where

data D = D Bool ()

class C a where
    wiggle :: a -> Bool

instance C D where
    wiggle (D b _) = b

f :: (C a) => a -> Bool
f x = wiggle x
