module ClassConstraints where

class C a where
    cee :: a

class (C a) => D a where
    dee :: a
