{-# LANGUAGE RecordWildCards #-}
module Wildcards where

import Prelude (DataTypeWithSelectors(DataTypeWithSelectors,selector1))

u :: DataTypeWithSelectors -> ()
u DataTypeWithSelectors{..} = let x = selector1 in ()

