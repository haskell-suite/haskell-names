{-# LANGUAGE RecordWildCards #-}
module WildcardsQualified where

import qualified Prelude as P (DataTypeWithSelectors(DataTypeWithSelectors,selector1))

u :: DataTypeWithSelectors -> ()
u P.DataTypeWithSelectors{..} = let x = selector1 in ()

