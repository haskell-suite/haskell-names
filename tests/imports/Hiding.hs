module Hiding where

import Prelude hiding
  ( selector2
  , Constructor1 -- should hide the *data* constructor
  , Newtype -- should hide both type and data constructor
  , NewtypeWithSelectors(..) -- hides the type and selectors
  , Class(method1) -- hides the class and the method
  )
