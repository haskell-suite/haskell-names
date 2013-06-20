module SelfReExport
  -- re-exports Prelude and locally defined entities
  ( module Prelude
  , module SelfReExport
  )
  where

-- After #31 is resolved, we should be able to replace this with an
-- ImplicitPrelude
import Prelude

data Local

local = local
