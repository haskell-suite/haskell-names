module ImportAll where

import Prelude (Newtype(..))

f :: () -> Newtype
f x = Newtype (f ())
